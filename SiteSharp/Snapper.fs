module Snapper

open WinInterop
open System
open System.Windows

let RegisterWindowMessage msg = int (RegisterWindowMessage msg)

// Message Constants.
let broadcastExistenceMessage = RegisterWindowMessage("sitesharp.newwindowopened")
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
    member w.SendMessage(toWin: nativeint, msg: 't -> unit, arg: 't) =
      w.SendMessage(toWin, fun() -> msg(arg))
   member w.SendMessage(toWin: Window, msg: Window -> unit) =
      let handle = toWin.Handle // marshal the window handle
      w.SendMessage(toWin, fun() -> msg(Window.FromHandle(handle)))
   member w.SendMessage(toWin: nativeint, msg: Window -> unit) =
      w.SendMessage(toWin, fun() -> msg(Window.FromHandle(toWin)))
   member w.SendMessage(toWin: nativeint, msg: nativeint -> unit) =
      w.SendMessage(toWin, fun() -> msg(toWin))

let log (msg: string) =
   try
      use writer = new IO.StreamWriter(new IO.FileStream("C:\\temp\\log.txt", IO.FileMode.Append, IO.FileAccess.Write)) //, IO.FileShare.ReadWrite))
      writer.WriteLine(msg)
   with _ -> ()

// log "---------------------------------------------------------------"
let logRect (r: Win32Rect) = log ("" + r.left.ToString() + ", " + r.top.ToString() + ", " + r.right.ToString() + ", " + r.bottom.ToString())

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
         // log ("received existence from " + wParam.ToString())
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
   // log ("sending existence " + window.Handle.ToString())
   let ``process`` = Diagnostics.Process.GetCurrentProcess()
   PostMessage(HWND_BROADCAST, uint32 broadcastExistenceMessage, window.Handle, nativeint ``process``.Id) |> ignore

let BroadcastDeathMessage (window: Window) =
   // log ("sending death " + window.Handle.ToString())
   let ``process`` = Diagnostics.Process.GetCurrentProcess()
   PostMessage(HWND_BROADCAST, uint32 broadcastDeathMessage, window.Handle, nativeint ``process``.Id) |> ignore

// Actual snapping support.
type WindowList = Collections.Generic.List<nativeint>

type Wind = North | East | South | West with
   override w.ToString() = match w with North -> "north" | East -> "east" | South -> "south" | West -> "west"

let opposite w =
   match w with 
      North -> South
    | East -> West
    | South -> North
    | West -> East

let wind, windows = fst, snd

let north w = match w with (north, _, _, _) -> north
let east w = match w with (_, east, _, _) -> east
let south w = match w with (_, _, south, _) -> south
let west w = match w with (_, _, _, west) -> west
let winds = [north; east; south; west]

let snappedWindows = (North, new WindowList()), (East, new WindowList()), (South, new WindowList()), (West, new WindowList())
let getSnapWindow wind =
   let fn = match wind with North -> north | East -> east | South -> south | West -> west
   fn snappedWindows

let win32RectsIntersect (rLeft: Win32Rect) (rRight: Win32Rect) =
   let test (l1, r1) (l2, r2) =
      if (l1 < l2) then r1 > l2
      else if (r1 > r2) then l1 < r2
      else false
   (test (rLeft.left, rLeft.right) (rRight.left, rRight.right)) &&
   (test (rLeft.top, rLeft.bottom) (rRight.top, rRight.bottom))

let windowsIntersect wLeft wRight =
   win32RectsIntersect (Window.Win32Rect(wLeft)) (Window.Win32Rect(wRight))

// Snap a window in the given wind.
let snapWindow wind windowHandle _ = (wind |> getSnapWindow |> windows).Add(windowHandle)
let enumerateSnappedWindows () =
   [north; east; south; west] |> List.map (fun f -> f snappedWindows |> windows) |> Seq.collect id // (fun winList -> winList)

// Brute force linear search, don't expect to hold too many of these windows.
let isSnapped (w: nativeint) =
   let lists = List.map (fun f -> f snappedWindows |> windows) [ north; east; south; west; ]
   List.exists (fun (ls: WindowList) -> ls.IndexOf(w) >= 0) lists

/// Location of target relative to source, e.g. if target is above source then North etc.
/// If the item is further North than it is East (say), the former is returned.
let relativeLocation source target =
   if (windowsIntersect source target) then None
   else
      let srcRect = Window.Win32Rect(source)
      let trgRect = Window.Win32Rect(target)
      let possibleLocations = [ 
         let d = srcRect.top - trgRect.bottom
         if (d > 0) then yield North, d
         let d = trgRect.left - srcRect.right
         if (d > 0) then yield East, d
         let d = trgRect.top - srcRect.bottom
         if (d > 0) then yield South, d
         let d = srcRect.left - trgRect.right
         if (d > 0) then yield West, d
      ]
      // Return the location with maximum distance
      Some (fst (List.fold (fun (w, max) (a, b) -> if b > max then (a, b) else (w, max)) (North, 0) possibleLocations))

let unsnap w =
   [north; east; south; west] |> List.map (fun f -> f snappedWindows |> windows) |> List.iter (fun list -> list.Remove(w) |> ignore)
let unsnapAll() = 
   [north; east; south; west] |> List.map (fun f -> f snappedWindows |> windows) |> List.iter (fun list -> list.Clear() |> ignore)

let mutable ignoreMove = false // ignore circular links
let rec moveAndMoveSnappedWindows x y (w: Window) =
   if ignoreMove then ()
   else
      w.Left <- w.Left + x
      w.Top <- w.Top + y
      ignoreMove <- true
      enumerateSnappedWindows() |> Seq.iter (fun sw -> w.SendMessage(sw, moveAndMoveSnappedWindows x y))

let allowMove () = ignoreMove <- false;

// Todo: this is not really correct as not all windows in the snap "group" are in fact snapped to the root window.
let AllowResize (rootWin: Window) location (amount: float) =
   let mutable rect = rootWin._Win32Rect
   match location with
   | North -> rect.top <- rect.top + int amount // todo: round?
   | East -> rect.right <- rect.right + int amount
   | South -> rect.bottom <- rect.bottom + int amount
   | West -> rect.left <- rect.left + int amount
   let r = rect
   // The resize is allowed if it does not intersect with any snapped window.
   not (enumerateSnappedWindows() |> Seq.exists (fun w -> win32RectsIntersect r (Window.Win32Rect w)))

let OnMoveRootWindow (rootWin: Window) x y =
   // See if we are about to "snap" to another window.
   let shiftKeyIsDown = Input.Keyboard.IsKeyDown(Input.Key.LeftShift) || Input.Keyboard.IsKeyDown(Input.Key.RightShift)
   let rootRect = rootWin._Win32Rect

   // If within this distance of another window, snap.
   let delta = 10

   // If within this distance of a corner of the window to snap to, align to this corner.
   let correctionDelta = 20

   if shiftKeyIsDown then
      enumerateSnappedWindows() |> Seq.iter (fun w -> rootWin.SendMessage(w, unsnap, rootWin.Handle))
      unsnapAll()
   else
      // Find the window we'll snap to.
      let snapTargets =
         otherSiteSharpWindows |>
            Seq.filter (fun w -> not(isSnapped w)) |> 
            Seq.choose (fun w ->
               let wRect = Window.Win32Rect w
               let location = relativeLocation (rootWin.Handle) w
               if location = None then None
               else
                  let shift, dir = 
                     match location.Value with
                     | North -> wRect.bottom - rootRect.top, location.Value
                     | South -> wRect.top - rootRect.bottom, location.Value
                     | East  -> wRect.left - rootRect.right, location.Value
                     | West  -> wRect.right - rootRect.left, location.Value
                  if (Math.Abs(shift) < delta) then
                     Some(shift, dir, w)
                  else None)

      // If there's nothing to snap or we intersect with another window, don't snap and move all snapped windows along.
      if Seq.isEmpty snapTargets || Seq.exists (fun w -> windowsIntersect rootWin.Handle w) otherSiteSharpWindows then
         ignoreMove <- true
         enumerateSnappedWindows() |> Seq.iter (fun w -> rootWin.SendMessage(w, moveAndMoveSnappedWindows x y)) 
      else
         let (shift, location, targetWin) = 
             // Choose the element with minimal shift (i.e. closest window)
             Seq.minBy (fun (s, _, _) -> s) snapTargets

         let wRect = Window.Win32Rect targetWin

         let round (a: int) (b: int) =
            let _a, _b = Math.Abs(a), Math.Abs(b)
            let min = if (_a < _b) then a else b
            if (Math.Abs(min) < correctionDelta) then float min
            else 0.

         let dx, dy = 
            if location = North || location = South then round (rootRect.left - wRect.left) (wRect.right - rootRect.right), float shift
            else float shift, round (rootRect.top - wRect.top) (wRect.bottom - rootRect.bottom)

         rootWin.SendMessage(targetWin, moveAndMoveSnappedWindows -dx -dy)

         snapWindow location targetWin rootWin
         rootWin.SendMessage(targetWin, snapWindow (opposite location) (rootWin.Handle))

      otherSiteSharpWindows |> Seq.iter (fun w -> rootWin.SendMessage(w, allowMove))
      ignoreMove <- false
   ()