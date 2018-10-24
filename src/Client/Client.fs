module Client

open System

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open Canvas
open Physics.Consts

open Shared

module Server =

    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IGameApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IGameApi>

module Canvas =
    let renderCircle (ctx: Context) style (ball: Matter.Body) =
        Canvas.Circle(ctx, style, ball.position.x, ball.position.y, ball.circleRadius)

    let renderShape (ctx: Context) style (shape: Matter.Body) =
        let vertices = shape.vertices |> Array.map (fun v -> v.x, v.y)
        Canvas.Shape(ctx, style, vertices)

    let renderSquare (ctx: Context) style size (x, y) =
        Canvas.Square(ctx, style, x, y, size)

    let renderLine (ctx: Context) style (x1, y1) (x2, y2) =
        Canvas.Line(ctx, style, x1, y1, x2, y2)

    let renderText (ctx: Context) style (x, y) text =
        Canvas.Text(ctx, style, text, x, y)

type Model =
    { Engine : Matter.Engine
      Player : Matter.Body
      Balls : Matter.Body [] }

type Msg =
    | Tick of delta : float

let init () =
    let engine, player, balls = Physics.init ()
    { Engine = engine
      Player = player
      Balls = balls }

let onTick (model: Model) delta =
    Physics.update model.Engine delta
    model

let update (model: Model) = function
    | Tick delta ->
        onTick model delta

let renderShape (ctx: Context) style (shape: Matter.Body) =
    let vertices = shape.vertices |> Array.map (fun v -> v.x, v.y)
    Canvas.Shape(ctx, style, vertices)

let view (model : Model) (ctx: Context) _ =
    let zoom =
        min
            (CANVAS_WIDTH / WORLD_WIDTH)
            (CANVAS_HEIGHT / WORLD_HEIGHT)
    ctx.clearRect(0., 0., CANVAS_WIDTH, CANVAS_HEIGHT)
    ctx.save()
    // Translate to the center
    ctx.translate(CANVAS_WIDTH / 2., CANVAS_HEIGHT / 2.)
    // Apply zoom
    ctx.scale(zoom, zoom)

    Canvas.renderShape ctx !^"yellow" model.Player

    ctx.restore()

let subscribe (canvas: Browser.HTMLCanvasElement) dispatch (model : Model) =
    canvas.width <- CANVAS_WIDTH
    canvas.height <- CANVAS_HEIGHT
    canvas.style.background <- "black"

[<Emit("$0 in $1")>]
let checkIn (listener: string) (o: obj) : bool = jsNative

Canvas.Start("canvas", init(), Tick, update, view, subscribe)
