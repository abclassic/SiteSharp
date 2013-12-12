(*
   Copyright Marcus van Houdt - 2013
 *)

module MainApp

open System
open System.Collections.Generic
open System.Threading
open System.Windows
open System.Windows.Media.Animation
open System.Windows.Controls
open FSharpx
open Microsoft.FSharp.Control.CommonExtensions
open Microsoft.FSharp.Control.WebExtensions
open System.Windows

type MainWindow = XAML<"MainWindow.xaml">

type UIElementCollection with
   member collection.RemoveAndAdd(e: UIElement) =
      if (collection.Contains(e)) then collection.Remove(e)
      collection.Add(e)

type Settings =
   { mutable url: string;
     mutable maxDataPoints: int;
   }
   static member Default = { url = null; maxDataPoints = 3600; }

type Metadata = None | Date of DateTime
type Entry = Point * Metadata

let entry p: Entry = p, None
let point (e: Entry) = match e with (p, _) -> p
let x e = (point e).X
let y e = (point e).Y

type GraphContext = { ScaleX: float; ScaleY: float; OffsetX: float; OffsetY: float  }

let mutable windowDragging = false
let mutable dragCoords = new Windows.Point()
let settings = Settings.Default
let tmpSettings = Settings.Default
let timeInterval = 1000 // setting?

let loadWindow() =
   let window = MainWindow()
   let origin = new Windows.Point(0., 0.)

   let drawPoint context entry =
      let ellipse = new Shapes.Ellipse()
      ellipse.Style <- window.Root.Resources.Item("point") :?> Style
      window.graph.Children.Add(ellipse) |> ignore
      Canvas.SetLeft(ellipse, (context.OffsetX + (x entry)) * context.ScaleX)
      Canvas.SetBottom(ellipse, (context.OffsetY + (y entry)) * context.ScaleY)

   let drawLine context left right shouldDrawPoint styleName =
      let line = new Shapes.Line()
      line.X1 <- 0.
      line.X2 <- ((x right) - (x left)) * context.ScaleX
      line.Y1 <- 0.
      line.Y2 <- ((y left) - (y right)) * context.ScaleY
      
      line.Style <- window.Root.Resources.Item(styleName) :?> Style

      Canvas.SetLeft(line, (context.OffsetX + (x left)) * context.ScaleX)
      Canvas.SetBottom(line, context.ScaleY * (context.OffsetY + if (y right) < (y left) then (y right) else (y left)))
      if (shouldDrawPoint) then
         drawPoint context right
      window.graph.Children.Add(line) |> ignore

   let makePath context entries =
      let path = new Shapes.Path()
      let h = window.graph.ActualHeight
      
      let makePoint (p: Point) = new Point(context.ScaleX * (p.X + context.OffsetX), h - context.ScaleY * (p.Y + context.OffsetY))
      let startPoint = makePoint (List.head entries |> point)

      // Map line segments to entries.
      let lineSegments = seq {
         for e in (List.tail entries) do
            yield new Media.LineSegment(e |> point |> makePoint, true), e
      }

      path.Data <- new Media.PathGeometry(seq { 
         yield new Media.PathFigure(startPoint, lineSegments |> Seq.map fst |> Seq.cast, false) 
      })

      Canvas.SetLeft(path, 0.0)
      Canvas.SetTop(path, 0.0)
      
      // Setup tooltip.
      let tooltipEllipse = new Shapes.Ellipse()
      tooltipEllipse.Style <- window.Root.Resources.Item("tooltipEllipse") :?> Style
      let label = new Controls.Label()
      label.Style <- window.Root.Resources.Item("tooltipLabel") :?> Style

      path.MouseEnter.Add(fun e -> 
         let p = path |> e.GetPosition

         // Find the relevant line segments where the mouse is.
         let leftSegment, rightSegment = 
            let firstSegment = Seq.head lineSegments
            if (p.X < (fst firstSegment).Point.X) then firstSegment, firstSegment
            else Seq.zip lineSegments (Seq.skip 1 lineSegments) |>
                 Seq.find (fun ((line, _), (nextLine, _)) -> (p.X > line.Point.X && p.X < nextLine.Point.X))

         // Determine if left or right segment is closest, and display that metadata.
         let segment = if (p.X - (fst leftSegment).Point.X) < ((fst rightSegment).Point.X - p.X) then 
                          leftSegment
                       else rightSegment
         let date = match segment with (_, (_, Date d)) -> d | _ -> failwith "unexpected metadata"
         let dateFormat = if (date.DayOfYear = DateTime.Now.DayOfYear) then "HH:mm:ss"
                          else "dd/MM/yyyy HH:mm:ss"

         label.Content <- Math.Round((h - p.Y) / context.ScaleY, 2).ToString() + " " + date.ToString(dateFormat)
         Canvas.SetLeft(tooltipEllipse, p.X)
         Canvas.SetTop(tooltipEllipse, p.Y)
         Canvas.SetLeft(label, p.X + tooltipEllipse.Width + 5. - window.graphScroller.HorizontalOffset)
         Canvas.SetTop(label, p.Y)
         Canvas.SetZIndex(label, Int32.MaxValue)

         // If label not visible, move it. Can this be done by wpf for us?
         label.Loaded.Add(fun _ -> 
            let lblPos = label.TransformToVisual(window.pane).Transform(origin)
            let r = lblPos.X + label.ActualWidth
            let d = r - (*window.graphScroller.HorizontalOffset - window.graphScroller.ViewportWidth *) window.pane.ActualWidth
            let t = if (d > 0.) then 
                       Canvas.SetLeft(label, r - d - label.ActualWidth)
                       Canvas.SetTop(label, lblPos.Y + 10.)
                       lblPos.Y + 10.
                    else
                       lblPos.Y
            if (t > window.graph.ActualHeight - label.ActualHeight) then
               Canvas.SetTop(label, t - label.ActualHeight - 10.))

         window.graph.Children.RemoveAndAdd(tooltipEllipse) |> ignore
         window.pane.Children
         window.pane.Children.RemoveAndAdd(label) |> ignore)

      tooltipEllipse.MouseLeave.Add(fun _ -> window.graph.Children.Remove(tooltipEllipse); window.pane.Children.Remove(label) |> ignore)

      path

   let drawGraph entries count =
      let minMaxLastPoint (minX, maxX, minY, maxY, _, start) (p: Point) =
         if (start) then
            (p.X, p.X, p.Y, p.Y, p, false)
         else
            Math.Min(p.X, minX), Math.Max(p.X, maxX), Math.Min(p.Y, minY), Math.Max(p.Y, maxY), p, false

      let w, h = window.graph.ActualWidth, window.graph.ActualHeight
      let minX, maxX, minY, maxY, lastPoint, _ = entries |> List.map point |> List.fold minMaxLastPoint (0., 0., 0., 0., origin, true)
      let avg =  ((count - 10), entries) ||> Seq.skip |> Seq.averageBy (fun p -> y p) // todo: better running avg
      let context = { ScaleX = (w - 10.) / window.graph.ActualWidth;
                      ScaleY = (h - 10.) / if minY = maxY then 1. else maxY
                      OffsetX = -minX;
                      OffsetY = 0. }
    
      window.graph.Children.Clear()
     
      if (window.MaxLabel.Content <> null) then
         let existingMax = Double.Parse(window.MaxLabel.Content :?> string)
         if (maxY > existingMax + 0.1) then
            WinInterop.BlinkWindow(window.Root) // blink only if not focused

      window.MinLabel.Content <- minY.ToString()
      window.AvgLabel.Content <- Math.Round(avg, 0).ToString()
      window.MaxLabel.Content <- maxY.ToString()
      window.LastLabel.Content <- lastPoint.Y.ToString() // todo: optimize to stop walking this thing
     
      drawLine context (entry (new Point(0., minY))) (entry (new Point(float maxX, minY))) false "average"
      drawLine context (entry (new Point(0., avg))) (entry (new Point(float maxX, avg))) false "average"
      drawLine context (entry (new Point(0., maxY))) (entry (new Point(float maxX, maxY))) false "average"
      
      let path = makePath context entries

       // Update the app icon.
      path.Loaded.Add(fun e ->
         let opacity = window.graph.Opacity
         window.graph.Opacity <- 1.
         let bitmap = new Media.Imaging.RenderTargetBitmap(int window.graph.ActualWidth, int window.graph.ActualHeight, 96., 96., Media.PixelFormats.Default)
         let scroll = int window.graphScroller.HorizontalOffset
         bitmap.Render(window.graph)
         window.graph.Opacity <- opacity
         let w = 100
         let h = 100
         let x = Math.Max(0, int ((lastPoint.X - minX) * context.ScaleX) - w - scroll)
         let y = Math.Max(0, int window.graph.ActualHeight - int (lastPoint.Y * context.ScaleY) - h / 2)
         let y = if y + h > int bitmap.Height then y - h / 2 else y
         let crop = new Media.Imaging.CroppedBitmap(bitmap, new Int32Rect(x, y, w, h))
         window.Root.Icon <- crop)

      window.graph.Children.Add(path) |> ignore
      drawPoint context (entry lastPoint)

      // Resize graph it grew.
      let scaleX = Math.Abs(maxX - minX) / w
      if (scaleX > 1.) then
         window.graph.Width <- scaleX * w
         if (window.graphScroller.HorizontalOffset + window.graphScroller.ViewportWidth > window.graph.ActualWidth - 5.) then // don't scroll if user has scrolled
            window.graphScroller.ScrollToRightEnd()

   let timer: Ref<Timer> = ref null

   let rec monitor d x dataPoints count context initialCall = Async.Start (async {
      do! Async.SwitchToContext(context)
      let url = settings.url
      do! Async.SwitchToThreadPool()   
      let client = new System.Net.WebClient()
      let watch = System.Diagnostics.Stopwatch.StartNew()
      let timestamp = DateTime.Now
      let! data = Async.Catch(client.AsyncDownloadString(new System.Uri(url)))
      let timeTaken = watch.ElapsedMilliseconds
      do! Async.SwitchToContext(context)
      if (not(initialCall) && !timer = null) then return () // restart      

      // Show error if there is one.
      let error = match data with Choice.Choice2Of2 e -> e | _ -> null
      if (error <> null) then
         System.Windows.MessageBox.Show(error.Message, "error", MessageBoxButton.OK, MessageBoxImage.Error) |> ignore
      
      let newDataPoints = if (error <> null) then dataPoints 
                          else let tmp = dataPoints @ [new Point(x, float timeTaken), Date(timestamp)] // is this O(1)?
                               if (count = settings.maxDataPoints) then List.tail tmp
                               else tmp
      let newCount = if (error <> null || count = settings.maxDataPoints) then count else count + 1
      if (newCount > 0) then drawGraph newDataPoints newCount |> ignore
      if (!timer <> null) then timer.Value.Dispose()
      timer := new Timer(new TimerCallback(fun _ -> monitor d (x + (float d / 50.)) newDataPoints newCount context false), null, d, 0)
   })

   let restartMonitor d =
      if (!timer <> null) then timer.Value.Dispose(); timer := null
      window.graph.Width <- window.graphScroller.Width
      monitor d 0. [] 0 SynchronizationContext.Current false

   // Hook settings dialog.
   let captureCurrentSettings (s) = s.url <- window.settingsUrl.Text
   let reinstatePrevSettings() = window.settingsUrl.Text <- tmpSettings.url
   window.thumbSettings.Click.Add(fun _ -> window.settings.Visibility <- Visibility.Visible; captureCurrentSettings(tmpSettings))
   window.settingsCancel.Click.Add(fun _ -> window.settings.Visibility <- Visibility.Hidden; reinstatePrevSettings())
   window.settingsOK.Click.Add(fun _ -> settings.url <- window.settingsUrl.Text
                                        window.settings.Visibility <- Visibility.Hidden
                                        restartMonitor timeInterval)
  
   captureCurrentSettings(settings)
   
   window.graph.Loaded.Add(fun e ->
      monitor timeInterval 0. [] 0 SynchronizationContext.Current true)

   window.Root.Loaded.Add(fun _ ->  WinInterop.MakeWindowTransparent(window.Root)) // still necessary?
  
   window.thumRestart.Click.Add(fun _ -> restartMonitor timeInterval)
   window.thumbMinimize.Click.Add(fun _ -> window.Root.WindowState <- WindowState.Minimized)
   window.thumbClose.Click.Add(fun _ -> window.Root.Close())
   window.graphScroller.PreviewMouseWheel.Add(fun e -> () |> (if (e.Delta > 0) then window.graphScroller.LineRight else window.graphScroller.LineLeft))
   window.graph.MouseDown.Add(fun e -> windowDragging <- true; dragCoords <- e.GetPosition(window.Root))
   window.graph.MouseUp.Add(fun _ -> windowDragging <- false)
   window.graph.MouseLeave.Add(fun _ -> windowDragging <- false)
   window.graph.MouseMove.Add(fun e -> if (windowDragging) then
                                          let p = e.GetPosition(window.Root)
                                          window.Root.Left <- window.Root.Left + p.X - dragCoords.X
                                          window.Root.Top <- window.Root.Top + p.Y - dragCoords.Y)

   let opacity = window.graph.Opacity
   window.Root.Activated.Add(fun _ -> window.graph.Opacity <- 1.)
   window.Root.Deactivated.Add(fun _ -> window.graph.Opacity <- opacity)

   // Suck, can't we do this in xaml, or? Todo: capture a part of the graph here (last x secs)
   let mgr = new System.Resources.ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
   let data = mgr.GetObject("AppIcon", null) :?> System.Drawing.Bitmap
   let rect = new Int32Rect(0, 0, data.Width, data.Height)
   window.Root.Icon <- System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(data.GetHbitmap(), IntPtr.Zero, rect, Media.Imaging.BitmapSizeOptions.FromEmptyOptions()) 
   window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore