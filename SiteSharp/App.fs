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

type GraphContext = { ScaleX: float; ScaleY: float; OffsetX: float; OffsetY: float  }

let loadWindow() =
   let window = MainWindow()
   
   let origin = { Point.x = 0.; y = 0. }

   let points = seq {
      for i in 0..20 do
         yield { Point.x = float (i * 20 + 20 + 5 * i); y = 2. * float (i * 5 - i * i) }
   }

   let drawPoint context point =
      let ellipse = new Shapes.Ellipse()
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
         //drawPoint context left
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
      // Canvas.SetBottom(path, context.ScaleY * (context.OffsetY + firstPoint.y))

      path

   let drawGraph w h points count =
      let minMaxPoint (minX, maxX, minY, maxY, start) p =
         if (start) then
            (p.x, p.x, p.y, p.y, false)
         else
            Math.Min(p.x, minX), Math.Max(p.x, maxX), Math.Min(p.y, minY), Math.Max(p.y, maxY), false

      let minX, maxX, minY, maxY, _ = Seq.fold minMaxPoint (0., 0., 0., 0., true) points
  
      let avg = Seq.averageBy (fun p -> p.y) (Seq.skip (count - 10) points)

      let context = { ScaleX = (w - 10.)/w; // (w - 10.) / Math.Abs(maxX - minX);
                      ScaleY = (h - 10.) / if minY = maxY then 1. else maxY // Math.Abs(maxY - minY);
                      OffsetX = 0.0; //-minX; 
                      OffsetY = -Math.Min(0., minY) } //-minY }
    
      window.graph.Children.Clear()
      let totalWidth = Math.Max(window.graph.ActualWidth, w)

      drawLine context {x = 0.; y = minY} {x = float totalWidth; y = minY } false "average"
      drawLine context {x = 0.; y = avg} {x = float totalWidth; y = avg } false "average"
      drawLine context {x = 0.; y = maxY} {x = float totalWidth; y = maxY } false "average"

      window.MinLabel.Content <- minY.ToString()
      window.AvgLabel.Content <- Math.Round(avg, 0).ToString()
      window.MaxLabel.Content <- maxY.ToString()
      
      window.graph.Children.Add(makePath context points) |> ignore
      drawPoint context (Seq.last points)
      
      // Resize graph it grew.
      let scaleX = Math.Abs(maxX - minX) / w
      if (scaleX > 1.) then
         window.graph.Width <- scaleX * w
         window.graphScroller.ScrollToRightEnd()

      //drawGraphInner origin points
      

   let timer: Ref<Timer> = ref null

   let rec demo w h points x =
      let newPoints = Seq.append points (Seq.singleton {x = x; y = 100. })
      if (!timer <> null) then timer.Value.Dispose()
      //window.graph.Children.Clear()
      drawGraph w h newPoints |> ignore
     
      let ctx = SynchronizationContext.Current
      timer := new Timer(new TimerCallback(fun _ -> ctx.Post(new SendOrPostCallback(fun _ -> demo w h newPoints (x + 100.)), null)), null, 1000, 0)
      ()

   let rec monitor w h d x (url: string) dataPoints count context = 
      Async.Start (async {
            do! Async.SwitchToThreadPool()   
            let client = new System.Net.WebClient()
            let watch = System.Diagnostics.Stopwatch.StartNew()
            let! data = client.AsyncDownloadString(new System.Uri(url))
            let timeTaken = watch.ElapsedMilliseconds
            do! Async.SwitchToContext(context)
            let newDataPoints = (Seq.append dataPoints (Seq.singleton {x = x; y = float timeTaken}))
            let newCount = count + 1
            drawGraph w h newDataPoints newCount |> ignore
            if (!timer <> null) then timer.Value.Dispose()
            timer := new Timer(new TimerCallback(fun _ -> monitor w h d (x + (float d / 10.)) url newDataPoints newCount context), null, d, 0)
         })
  
  
   //let lastPoint = drawGraph context points
   

   window.graph.Loaded.Add(fun e ->
      let w, h = window.graph.RenderSize.Width, window.graph.RenderSize.Height


      //drawGraph w h points |> ignore
      // demo w h (Seq.singleton origin) 10.0
      monitor w h 1000 0. "http://zoeken.provant.bibliotheek.be/?q=boek" Seq.empty 0 SynchronizationContext.Current
      )

   window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore