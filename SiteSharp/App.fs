(*
   Copyright Marcus van Houdt - 9/12/2013
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

type Settings =
   { mutable url: string;
     mutable maxDataPoints: int;
   }
   static member Default = { url = null; maxDataPoints = 3600; }
   member s.CopyTo other =
      other.url <- s.url
      other.maxDataPoints <- s.maxDataPoints

type GraphContext = { ScaleX: float; ScaleY: float; OffsetX: float; OffsetY: float  }

let mutable windowDragging = false
let mutable dragCoords = new Windows.Point()
let settings = Settings.Default
let tmpSettings = Settings.Default

let loadWindow() =
   let window = MainWindow()
   let origin = new Windows.Point(0., 0.)

   let drawPoint context (point: Point) =
      let ellipse = new Shapes.Ellipse()
      ellipse.Style <- window.Root.Resources.Item("point") :?> Style
      window.graph.Children.Add(ellipse) |> ignore
      Canvas.SetLeft(ellipse, (context.OffsetX + point.X) * context.ScaleX)
      Canvas.SetBottom(ellipse, (context.OffsetY + point.Y) * context.ScaleY)

   let drawLine context (left: Point) (right: Point) shouldDrawPoint styleName =
      let line = new Shapes.Line()
      line.X1 <- 0.
      line.X2 <- (right.X - left.X) * context.ScaleX
      line.Y1 <- 0.
      line.Y2 <- (left.Y - right.Y) * context.ScaleY
      
      line.Style <- window.Root.Resources.Item(styleName) :?> Style

      Canvas.SetLeft(line, (context.OffsetX + left.X) * context.ScaleX)
      Canvas.SetBottom(line, context.ScaleY * (context.OffsetY + if right.Y < left.Y then right.Y else left.Y))
      if (shouldDrawPoint) then
         drawPoint context right
      window.graph.Children.Add(line) |> ignore

   let makePath context (points: Point list) =
      let path = new Shapes.Path()
      let h = window.graph.ActualHeight
      let firstPoint = List.head points

      let scalePoint (p: Point) = new Point(p.X * context.ScaleX, h - p.Y * context.ScaleY)
      let startPoint = scalePoint (new Point(firstPoint.X + context.OffsetX, firstPoint.Y + context.OffsetY))

      path.Data <- new Media.PathGeometry(seq {
            let figure = new Media.PathFigure()
            figure.IsClosed <- false
            figure.StartPoint <- startPoint
            for p in (List.tail points) do
               new Media.LineSegment(scalePoint p, true) |> figure.Segments.Add
            yield figure
         });

      Canvas.SetLeft(path, 0.0)
      Canvas.SetTop(path, 0.0)
      
      let tooltipEllipse = new Shapes.Ellipse()
      tooltipEllipse.Style <- window.Root.Resources.Item("tooltipEllipse") :?> Style
      let label = new Controls.Label()
      label.Style <- window.Root.Resources.Item("tooltipLabel") :?> Style

      path.MouseEnter.Add(fun e -> let p = path |> e.GetPosition
                                   label.Content <- Math.Round((h - p.Y) / context.ScaleY, 2).ToString()
                                   Canvas.SetLeft(tooltipEllipse, p.X)
                                   Canvas.SetTop(tooltipEllipse, p.Y)
                                   Canvas.SetLeft(label, p.X + tooltipEllipse.Width + 5.)
                                   Canvas.SetTop(label, p.Y)
                                   window.graph.Children.Add(tooltipEllipse) |> ignore
                                   window.graph.Children.Add(label) |> ignore)

      tooltipEllipse.MouseLeave.Add(fun _ -> window.graph.Children.Remove(tooltipEllipse); window.graph.Children.Remove(label) |> ignore)

      path

   let drawGraph points count =
      let minMaxLastPoint (minX, maxX, minY, maxY, _, start) (p: Point) =
         if (start) then
            (p.X, p.X, p.Y, p.Y, p, false)
         else
            Math.Min(p.X, minX), Math.Max(p.X, maxX), Math.Min(p.Y, minY), Math.Max(p.Y, maxY), p, false

      let w, h = window.graph.ActualWidth, window.graph.ActualHeight
      let minX, maxX, minY, maxY, lastPoint, _ = List.fold minMaxLastPoint (0., 0., 0., 0., origin, true) points
      let avg =  ((count - 10), points) ||> Seq.skip |> Seq.averageBy (fun p -> p.Y) // todo: better running avg
      let context = { ScaleX = (w - 10.) / window.graph.ActualWidth;
                      ScaleY = (h - 10.) / if minY = maxY then 1. else maxY
                      OffsetX = 0.0;
                      OffsetY = -Math.Min(0., minY) }
    
      window.graph.Children.Clear()
     
      if (window.MaxLabel.Content <> null) then
         let existingMax = Double.Parse(window.MaxLabel.Content :?> string)
         if (maxY > existingMax + 0.1) then
            WinInterop.BlinkWindow(window.Root) // blink only if not focused

      window.MinLabel.Content <- minY.ToString()
      window.AvgLabel.Content <- Math.Round(avg, 0).ToString()
      window.MaxLabel.Content <- maxY.ToString()
      window.LastLabel.Content <- lastPoint.Y.ToString() // todo: optimize to stop walking this thing
     
      drawLine context (new Point(0., minY)) (new Point(float maxX, minY)) false "average"
      drawLine context (new Point(0., avg)) (new Point(float maxX, avg)) false "average"
      drawLine context (new Point(0., maxY)) (new Point(float maxX, maxY)) false "average"
       
      let path = makePath context points
      window.graph.Children.Add(path) |> ignore
      drawPoint context lastPoint

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
      let! data = Async.Catch(client.AsyncDownloadString(new System.Uri(url)))
      let timeTaken = watch.ElapsedMilliseconds
      do! Async.SwitchToContext(context)
      if (not(initialCall) && !timer = null) then return () // restart      

      // Show error if there is one.
      let error = match data with Choice.Choice2Of2 e -> e | _ -> null
      if (error <> null) then
         System.Windows.MessageBox.Show(error.Message, "error", MessageBoxButton.OK, MessageBoxImage.Error) |> ignore
      
      let newDataPoints = if (error <> null) then dataPoints 
                          else let tmp = dataPoints @ [new Point(x, float timeTaken)] // is this O(1)?
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

   let timeInterval = 1000

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
   window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore