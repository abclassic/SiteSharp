module MainApp

open System
open System.Threading
open System.Windows
open System.Windows.Media.Animation
open System.Windows.Controls
open FSharpx
open Microsoft.FSharp.Control.CommonExtensions
open Microsoft.FSharp.Control.WebExtensions
open System.Windows

type MainWindow = XAML<"MainWindow.xaml">
type Point = 
   {x: float; y: float }
   static member (+) (a, b) =
      {x = a.x + b.x; y = a.y + b.y}
   member p.AsWpfPoint() =
      new Windows.Point(p.x, p.x)

type Settings = 
   { mutable url: string;
     mutable updated: bool
   }

type GraphContext = { ScaleX: float; ScaleY: float; OffsetX: float; OffsetY: float  }

let mutable windowDragging = false
let mutable dragCoords = new Windows.Point()
let settings = { url = null; updated = false }
let tmpSettings = { url = null; updated = false }

let loadWindow() =
   let window = MainWindow()
   let origin = { Point.x = 0.; y = 0. }

   let points = seq {
      for i in 0..20 do
         yield { Point.x = float (i * 20 + 20 + 5 * i); y = 2. * float (i * 5 - i * i) }
   }

   let drawPoint context point =
      let ellipse = new Shapes.Ellipse()
      ellipse.Style <- window.Root.Resources.Item("point") :?> Style
      window.graph.Children.Add(ellipse) |> ignore
      Canvas.SetLeft(ellipse, (context.OffsetX + point.x) * context.ScaleX)
      Canvas.SetBottom(ellipse, (context.OffsetY + point.y) * context.ScaleY)

   let drawLine context left right shouldDrawPoint styleName =
      let line = new Shapes.Line()
      line.X1 <- 0.
      line.X2 <- (right.x - left.x) * context.ScaleX
      line.Y1 <- 0.
      line.Y2 <- (left.y - right.y) * context.ScaleY
      
      line.Style <- window.Root.Resources.Item(styleName) :?> Style

      Canvas.SetLeft(line, (context.OffsetX + left.x) * context.ScaleX)
      Canvas.SetBottom(line, context.ScaleY * (context.OffsetY + if right.y < left.y then right.y else left.y))
      if (shouldDrawPoint) then
         drawPoint context right
      window.graph.Children.Add(line) |> ignore

   let makePath context points =
      let path = new Shapes.Path()
      let h = window.graph.ActualHeight
      let firstPoint = Seq.head points

      let scalePoint p = new Windows.Point(p.x * context.ScaleX, h - p.y * context.ScaleY)
      let startPoint = scalePoint { x = firstPoint.x + context.OffsetX; y = firstPoint.y + context.OffsetY}

      path.Data <- new Media.PathGeometry(seq {
            let figure = new Media.PathFigure()
            figure.IsClosed <- false
            figure.StartPoint <- startPoint //scalePoint (Seq.head points)
            for p in (Seq.skip 1 points) do
               let line = new Media.LineSegment(scalePoint p, true)
               figure.Segments.Add(line);

            yield figure
         });

      Canvas.SetLeft(path, 0.0)
      Canvas.SetTop(path, 0.0)
      
      let tooltipEllipse = new Shapes.Ellipse()
      tooltipEllipse.Style <- window.Root.Resources.Item("tooltipEllipse") :?> Style
      let label = new Controls.Label()
      label.Style <- window.Root.Resources.Item("tooltipLabel") :?> Style

      path.MouseEnter.Add(fun e -> let p = e.GetPosition(path)
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
      let minMaxPoint (minX, maxX, minY, maxY, start) p =
         if (start) then
            (p.x, p.x, p.y, p.y, false)
         else
            Math.Min(p.x, minX), Math.Max(p.x, maxX), Math.Min(p.y, minY), Math.Max(p.y, maxY), false

      let w, h = window.graph.ActualWidth, window.graph.ActualHeight
      let minX, maxX, minY, maxY, _ = Seq.fold minMaxPoint (0., 0., 0., 0., true) points
  
      let avg = Seq.averageBy (fun p -> p.y) (Seq.skip (count - 10) points)

      let context = { ScaleX = (w - 10.) / window.graph.ActualWidth; // (w - 10.) / Math.Abs(maxX - minX);
                      ScaleY = (h - 10.) / if minY = maxY then 1. else maxY // Math.Abs(maxY - minY);
                      OffsetX = 0.0; //-minX; 
                      OffsetY = -Math.Min(0., minY) } //-minY }
    
      window.graph.Children.Clear()
     
      if (window.MaxLabel.Content <> null) then
         let existingMax = Double.Parse(window.MaxLabel.Content :?> string)
         if (maxY > existingMax + 0.1) then
            WinInterop.BlinkWindow(window.Root) // blink only if not focused

      window.MinLabel.Content <- minY.ToString()
      window.AvgLabel.Content <- Math.Round(avg, 0).ToString()
      window.MaxLabel.Content <- maxY.ToString()
      window.LastLabel.Content <- (Seq.last points).y.ToString() // todo: optimize to stop walking this thing
     
      drawLine context {x = 0.; y = minY} {x = float maxX; y = minY } false "average"
      drawLine context {x = 0.; y = avg} {x = float maxX; y = avg } false "average"
      drawLine context {x = 0.; y = maxY} {x = float maxX; y = maxY } false "average"
       
      let path = makePath context points
      window.graph.Children.Add(path) |> ignore
      drawPoint context (Seq.last points)

      // Resize graph it grew.
      let scaleX = Math.Abs(maxX - minX) / w
      if (scaleX > 1.) then
         window.graph.Width <- scaleX * w
         let infox = window.graphScroller.HorizontalOffset
         let infow = window.graphScroller.ViewportWidth
         let infod = window.graph.ActualWidth
         if (window.graphScroller.HorizontalOffset + window.graphScroller.ViewportWidth > window.graph.ActualWidth - 5.) then // don't scroll if user has scrolled
            window.graphScroller.ScrollToRightEnd()
      

   let timer: Ref<Timer> = ref null

   let rec demo w h points x =
      let newPoints = Seq.append points (Seq.singleton {x = x; y = 100. })
      if (!timer <> null) then timer.Value.Dispose()
      drawGraph newPoints |> ignore
     
      let ctx = SynchronizationContext.Current
      timer := new Timer(new TimerCallback(fun _ -> ctx.Post(new SendOrPostCallback(fun _ -> demo w h newPoints (x + 100.)), null)), null, 1000, 0)
      ()

   let getUrl() = settings.url

   let rec monitor d x dataPoints count context = 
      if (settings.updated) then
         settings.updated <- false
         monitor d 0. (Seq.empty) 0 context
      else
         Async.Start (async {
               do! Async.SwitchToContext(context)
               let url = getUrl()
               do! Async.SwitchToThreadPool()   
               let client = new System.Net.WebClient()
               let watch = System.Diagnostics.Stopwatch.StartNew()
               let! data = client.AsyncDownloadString(new System.Uri(url))
               let timeTaken = watch.ElapsedMilliseconds
               do! Async.SwitchToContext(context)
               let newDataPoints = (Seq.append dataPoints (Seq.singleton {x = x; y = float timeTaken}))
               let newCount = count + 1
               drawGraph newDataPoints newCount |> ignore
               if (!timer <> null) then timer.Value.Dispose()
               timer := new Timer(new TimerCallback(fun _ -> monitor d (x + (float d / 50.)) newDataPoints newCount context), null, d, 0)
            })

   // Hook settings dialog.
   let captureCurrentSettings (s) = s.url <- window.settingsUrl.Text
   let reinstatePrevSettings() = window.settingsUrl.Text <- tmpSettings.url
   window.thumbSettings.Click.Add(fun _ -> window.settings.Visibility <- Visibility.Visible; captureCurrentSettings(tmpSettings))
   window.settingsCancel.Click.Add(fun _ -> window.settings.Visibility <- Visibility.Hidden; reinstatePrevSettings())
   window.settingsOK.Click.Add(fun _ -> settings.url <- tmpSettings.url; settings.updated <- true; window.settings.Visibility <- Visibility.Hidden)
  
   captureCurrentSettings(settings)
   
   window.graph.Loaded.Add(fun e ->
      monitor 1000 0. Seq.empty 0 SynchronizationContext.Current)

   window.Root.Loaded.Add(fun _ ->  WinInterop.MakeWindowTransparent(window.Root))   
  
   window.thumbClose.Click.Add(fun _ -> window.Root.Close())
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