module WinInterop

open Microsoft.FSharp.Core.Operators
open System
open System.Runtime.InteropServices
open System.Windows
open System.Windows.Interop
open Microsoft.FSharp.NativeInterop
open Microsoft.FSharp.Reflection
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

[<StructLayout(LayoutKind.Sequential)>]
[<Struct>]
type Margins =
   struct
      val mutable cxLeftWidth: int
      val mutable cxRightWidth: int
      val mutable cyTopHeight: int
      val mutable cyBottomHeight: int
   end

type DWORD = uint32
let DWORD d = uint32 d

[<StructLayout(LayoutKind.Sequential)>]
type FLASHWINFO =
   struct
      val mutable cbSize: uint32
      val mutable hwnd: nativeint
      val mutable dwFlags : uint32
      val mutable uCount: uint32
      val mutable dwTimeout: uint32
   end

//[<Class>]
//type BytePtrStream(len: int, ptr: nativeptr<byte>) =
//   inherit IO.Stream()
//   let mutable remaining = len
//   override stream.Read(buffer: byte[], offset: int, count: int) =
//      let mutable numRead = 0
//      for i = offset to offset + count - 1 do
//         byte[i] <- 0
//      

[<StructLayout(LayoutKind.Sequential)>]
type CopyDataStruct =
   struct
      val mutable dwData: nativeint
      val mutable cbData: DWORD
      val mutable lpData: nativeint
   end

type Window with
   member window.Handle =
      let source = PresentationSource.FromVisual(window) :?> HwndSource
      source.Handle

type Flashing = 
   | Stop = 0
   | Caption = 1
   | Tray = 2
   | All = 3
   | Timer = 4
   | TimerNoFg = 12

[<DllImport("user32.dll")>]
extern [<MarshalAs(UnmanagedType.Bool)>]bool FlashWindowEx(FLASHWINFO* pwfi);

[<DllImport("DwmApi.dll")>]
extern int DwmExtendFrameIntoClientArea(nativeint hwnd, Margins* pMarInset)

let BlinkWindow (w: Window) =
   let mutable flash = new FLASHWINFO()
   flash.cbSize <- uint32 sizeof<FLASHWINFO>
   flash.hwnd <- (new WindowInteropHelper(w)).Handle
   flash.dwFlags <- uint32 (Flashing.All ||| Flashing.TimerNoFg)
   flash.uCount <- 10u
   flash.dwTimeout <- 0u
   FlashWindowEx(&&flash) |> ignore

let MakeWindowTransparent (w: Window) =
   let windowPointer = (new WindowInteropHelper(w)).Handle
   let windowSource = HwndSource.FromHwnd(windowPointer)
   if (windowSource <> null) then
      if (windowSource <> null && windowSource.CompositionTarget <> null) then
         windowSource.CompositionTarget.BackgroundColor <- System.Windows.Media.Color.FromArgb(0uy, 0uy, 0uy, 0uy)
      let mutable margins = new Margins()
      margins.cxLeftWidth <- 0
      margins.cxRightWidth <- Convert.ToInt32(w.Width) * Convert.ToInt32(w.Width)
      margins.cyTopHeight <- 0
      margins.cyBottomHeight <-  Convert.ToInt32(w.Height) * Convert.ToInt32(w.Height)

      DwmExtendFrameIntoClientArea(windowSource.Handle, &&margins) |> ignore
   ()


// Win32 messaging support.

[<DllImport("user32.dll")>]
extern uint32 RegisterWindowMessage(string message)

[<DllImport("user32.dll")>]
extern bool ShowWindow(nativeint hWnd, int show)

[<DllImport("user32.dll")>]
extern nativeint SendMessage(nativeint hwnd, uint32 msg, nativeint wParam, nativeint lParam)

[<DllImport("user32.dll")>]
extern nativeint PostMessage(nativeint hwnd, uint32 msg, nativeint wParam, nativeint lParam)

[<DllImport("kernel32.dll")>]
extern bool DuplicateHandle(nativeint sourceProcessHandle, nativeint sourceHandle, nativeint targetProcessHandle, nativeint* targetHandle, DWORD desiredAccess, bool inheritHandle, DWORD options)

let HWND_BROADCAST = nativeint 0xffff
let WM_COPYDATA = 0x004A

//type MessageId = NewWindow
//
//type Message = {
//      window: Window;
//      id: int
//   }
//
//let NewWindow w = { window = w; id = 0; }
//
//let (=>) (window, messageId, payload: obj option) (targetWindow: Window) =
//   ()
//
//let w: Window = null
//
//(NewWindow, w) => w

/// Serializes the given object from the source to the target window.
let SendData (sourceWindow: nativeint) (window: nativeint) (data: obj) =
   let formatter = new BinaryFormatter()
   use stream = new IO.MemoryStream()
   formatter.Serialize(stream, data)
   stream.Flush()
   let buffer = stream.GetBuffer()
   let pinBuffer = GCHandle.Alloc(buffer, GCHandleType.Pinned)
   let mutable copyDataStruct = new CopyDataStruct()
   copyDataStruct.dwData <- nativeint 0
   copyDataStruct.cbData <- DWORD stream.Length
   copyDataStruct.lpData <- Marshal.UnsafeAddrOfPinnedArrayElement(buffer, 0)
   SendMessage(window, uint32 WM_COPYDATA, sourceWindow, NativePtr.toNativeInt (&&copyDataStruct))
   pinBuffer.Free()

let ReceiveData<'Data> (data: nativeint) : 'Data =
   let copyData = Marshal.PtrToStructure(data, typeof<CopyDataStruct>) :?> CopyDataStruct
   let buffer = Array.zeroCreate<byte>(int copyData.cbData)
   let dataPtr = NativePtr.ofNativeInt<byte> (copyData.lpData)
   for i = 0 to buffer.Length - 1 do
      buffer.[i] <- NativePtr.get dataPtr i
   let formatter = new BinaryFormatter()
   use stream = new IO.MemoryStream(buffer)
   formatter.Deserialize(stream) :?> 'Data

// Message Constants.
let broadcastExistenceMessage = int (RegisterWindowMessage("sitesharp.newwindowopened")) //Messages.NewWindowOpened |> msgStr))
let existenceAckMessage = int (RegisterWindowMessage("sitesharp.newwindowack"))
let minimizeWindowsMessage = int (RegisterWindowMessage("sitesharp.minimizewindows")) //Messages.MinimizeWindows |> msgStr))
let broadcastDeathMessage = int (RegisterWindowMessage("sitesharp.windowdied"))
let helloWorldMessage = int (RegisterWindowMessage("sitesharp.helloworld"))

let otherSiteSharpWindows = new System.Collections.Generic.List<nativeint>()

let MessageHook (hwnd: nativeint) (msg: int) (wParam: nativeint) (lParam: nativeint) (handled : byref<bool>) =
   
   match msg with
   | msg when msg = WM_COPYDATA ->
      let data = ReceiveData<string> lParam
      MessageBox.Show(data) |> ignore
   | msg when msg = broadcastDeathMessage ->
      if (wParam <> hwnd) then // other window sent broadcast
         otherSiteSharpWindows.Remove(wParam) |> ignore
      handled <- true
   | msg when msg = existenceAckMessage ->
      otherSiteSharpWindows.Add(wParam)
      handled <- true
   | msg when msg = broadcastExistenceMessage ->
      let sourceWindowHandle = wParam
      let sourceProcessHandle = Diagnostics.Process.GetProcessById(int lParam).Handle
      let mutable targetHandle = nativeint 0
      let currentProcessHandle =  Diagnostics.Process.GetCurrentProcess().Handle
      if (sourceWindowHandle <> hwnd) then // other window sent this
         otherSiteSharpWindows.Add(sourceWindowHandle)
         SendMessage(sourceWindowHandle, uint32 existenceAckMessage, hwnd, nativeint 0) |> ignore
         //MessageBox.Show("Hi") |> ignore
//         let b = DuplicateHandle(sourceProcessHandle, sourceWindowHandle, currentProcessHandle, &&targetHandle, 0u, false, 2u)
//         let error = Marshal.GetLastWin32Error()
       //  ShowWindow(sourceWindowHandle, 11) |> ignore
      //   MessageBox.Show("BROADCAST " + msg.ToString() + " " + b.ToString() + " " + error.ToString()) |> ignore
      handled <- true
   | msg when msg = minimizeWindowsMessage ->
      ShowWindow(hwnd, 11) |> ignore
   | _ -> handled <- false
   IntPtr.Zero

// Hook the win32 message loop.
let SendMessageHook (window: Window) =
   let source = PresentationSource.FromVisual(window) :?> HwndSource
   source.AddHook(new HwndSourceHook(fun (hwnd: nativeint) (msg: int) (wParam: nativeint) (lParam: nativeint) (handled : byref<bool>) ->
      MessageHook hwnd msg wParam lParam &handled))

let BroadcastExistenceMessage (window: Window) =
   let source = PresentationSource.FromVisual(window) :?> HwndSource
   let ``process`` = Diagnostics.Process.GetCurrentProcess()
   PostMessage(HWND_BROADCAST, uint32 broadcastExistenceMessage, source.Handle, nativeint ``process``.Id) |> ignore

let BroadcastDeathMessage (window: Window) =
   let source = PresentationSource.FromVisual(window) :?> HwndSource
   let ``process`` = Diagnostics.Process.GetCurrentProcess()
   PostMessage(HWND_BROADCAST, uint32 broadcastDeathMessage, source.Handle, nativeint ``process``.Id) |> ignore



//let TEMPCLICK(w) =
//   BroadcastMessage (broadcastExistenceMessage) w |> ignore

let HideAllOthers(sourceWindow: Window) =
   for w in otherSiteSharpWindows do
      SendData (sourceWindow.Handle) w "foobar"
      //SendMessage(w, uint32 minimizeWindowsMessage, nativeint 0, nativeint 0) |> ignore

//// Dynamic sendmessage support.
//let (?) (window: Window) (name: string) : 'R =
//   let rtype = typeof<'R>
//   if not (FSharpType.IsFunction rtype) then failwith name + " in ?" + name + " should be a function"
//   failwith "foob"
//
//type Message = Dummy of Window



