module WinInterop

open Microsoft.FSharp.Core.Operators
open System
open System.Runtime.InteropServices
open System.Windows
open System.Windows.Interop
open Microsoft.FSharp.NativeInterop


[<StructLayout(LayoutKind.Sequential)>]
[<Struct>]
type Margins =
   struct
      val mutable cxLeftWidth: int
      val mutable cxRightWidth: int
      val mutable cyTopHeight: int
      val mutable cyBottomHeight: int
   end

[<StructLayout(LayoutKind.Sequential)>]
type FLASHWINFO =
   struct
      val mutable cbSize: uint32
      val mutable hwnd: nativeint
      val mutable dwFlags : uint32
      val mutable uCount: uint32
      val mutable dwTimeout: uint32
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