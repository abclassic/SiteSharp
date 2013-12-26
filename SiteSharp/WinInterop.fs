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

type Window with
   member window.Handle = (PresentationSource.FromVisual(window) :?> HwndSource).Handle
   static member FromHandle (handle: nativeint) = HwndSource.FromHwnd(handle).RootVisual :?> Window

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
   SendMessage(window, uint32 WM_COPYDATA, sourceWindow, NativePtr.toNativeInt (&&copyDataStruct)) |> ignore
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

// Hook the win32 message loop.
let SendMessageHook (window: Window) (messageHook: nativeint -> int -> nativeint -> nativeint -> bool * nativeint)  =
   let source = PresentationSource.FromVisual(window) :?> HwndSource
   source.AddHook(new HwndSourceHook(fun (hwnd: nativeint) (msg: int) (wParam: nativeint) (lParam: nativeint) (handled : byref<bool>) ->
      let h, r = messageHook hwnd msg wParam lParam
      handled <- h; r))

      //SendMessage(w, uint32 minimizeWindowsMessage, nativeint 0, nativeint 0) |> ignore

//// Dynamic sendmessage support.
//let (?) (window: Window) (name: string) : 'R =
//   let rtype = typeof<'R>
//   if not (FSharpType.IsFunction rtype) then failwith name + " in ?" + name + " should be a function"
//   failwith "foob"
//
//type Message = Dummy of Window



