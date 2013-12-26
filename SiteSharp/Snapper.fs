module Snapper

open WinInterop
open System
open System.Windows

let RegisterWindowMessage msg = int (RegisterWindowMessage msg)

// Message Constants.
let broadcastExistenceMessage = RegisterWindowMessage("sitesharp.newwindowopened") //Messages.NewWindowOpened |> msgStr))
let existenceAckMessage = RegisterWindowMessage("sitesharp.newwindowack")
let broadcastDeathMessage = RegisterWindowMessage("sitesharp.windowdied")

let otherSiteSharpWindows = new System.Collections.Generic.List<nativeint>()

/// Window augmentation to send messages in the form of (serialized) functions.
type Window with
   member w.SendMessage(toWin: nativeint, msg: unit -> unit) =
      SendData (w.Handle) toWin msg
   member w.SendMessage(toWin: Window, msg: unit -> unit) =
      SendData (w.Handle) (toWin.Handle) msg
   member w.SendMessage(toWin: Window, msg: 't -> unit, arg: 't) =
      w.SendMessage(toWin, fun() -> msg(arg))
   member w.SendMessage(toWin: Window, msg: Window -> unit) =
      let handle = toWin.Handle // marshal the window handle
      w.SendMessage(toWin, fun() -> msg(Window.FromHandle(handle)))
   member w.SendMessage(toWin: nativeint, msg: Window -> unit) =
      w.SendMessage(toWin, fun() -> msg(Window.FromHandle(toWin)))


let MessageHook (hwnd: nativeint) (msg: int) (wParam: nativeint) (lParam: nativeint) =
   let handled =
      match msg with
      // Copydata is used to serialize a function and execute it.
      | msg when msg = WM_COPYDATA ->
         let data = ReceiveData<unit -> unit> lParam
         data(); true
      | msg when msg = broadcastDeathMessage ->
         if (wParam <> hwnd) then // other window sent broadcast
            otherSiteSharpWindows.Remove(wParam) |> ignore
         true
      | msg when msg = existenceAckMessage ->
         otherSiteSharpWindows.Add(wParam)
         true
      | msg when msg = broadcastExistenceMessage ->
         let sourceWindowHandle = wParam
         let sourceProcessHandle = Diagnostics.Process.GetProcessById(int lParam).Handle
         let mutable targetHandle = nativeint 0
         let currentProcessHandle =  Diagnostics.Process.GetCurrentProcess().Handle
         if (sourceWindowHandle <> hwnd) then // other window sent this
            otherSiteSharpWindows.Add(sourceWindowHandle)
            SendMessage(sourceWindowHandle, uint32 existenceAckMessage, hwnd, nativeint 0) |> ignore
         true
      | _ -> false 

   handled, nativeint 0


let BroadcastExistenceMessage (window: Window) =
   let ``process`` = Diagnostics.Process.GetCurrentProcess()
   PostMessage(HWND_BROADCAST, uint32 broadcastExistenceMessage, window.Handle, nativeint ``process``.Id) |> ignore

let BroadcastDeathMessage (window: Window) =
   let ``process`` = Diagnostics.Process.GetCurrentProcess()
   PostMessage(HWND_BROADCAST, uint32 broadcastDeathMessage, window.Handle, nativeint ``process``.Id) |> ignore

let HideAllOthers(sourceWindow: Window) =
   for w in otherSiteSharpWindows do
      SendData (sourceWindow.Handle) w (fun () -> MessageBox.Show("foobar") |> ignore)

let moveWindow x y (w: Window)=
   w.Left <- w.Left + x
   w.Top <- w.Top + y

let MoveAllOthers (sourceWin: Window) x y =
   for w in otherSiteSharpWindows do
      sourceWin.SendMessage(w, (moveWindow x y))