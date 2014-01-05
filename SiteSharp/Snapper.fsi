
namespace global
module Snapper =

   open System.Windows

   type Wind = North | East | South | West
   val OnMoveRootWindow: Window -> float -> float -> unit
   val AllowResize: Window -> Wind -> float -> bool

   val BroadcastExistenceMessage: Window -> unit
   val BroadcastDeathMessage: Window -> unit

   val MessageHook: nativeint -> int -> nativeint -> nativeint -> bool * nativeint