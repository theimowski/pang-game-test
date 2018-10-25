module Client

open System

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

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

type Dir =
    | Left
    | Right

type Model =
    { Engine : Matter.Engine
      Player : Matter.Body
      Balls : Matter.Body []
      MoveDir : Dir option }

type Msg =
    | Tick of delta : float
    | Move of Dir option

let init () =
    let engine, player, balls = Physics.init ()
    { Engine = engine
      Player = player
      Balls = balls
      MoveDir = None }

let movePlayer player dir =
    let x = match dir with Left -> -PLAYER_X_FORCE | Right -> PLAYER_X_FORCE
    Physics.moveHorizontally player x

let onTick (model: Model) delta =
    Physics.update model.Engine delta

    Option.iter (movePlayer model.Player) model.MoveDir

    model

let update (model: Model) = function
    | Tick delta ->
        onTick model delta
    | Move dir  ->
        { model with MoveDir = dir }

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

open Fable.Import.Browser

let subscribe (canvas: Browser.HTMLCanvasElement) dispatch (model : Model) =
    canvas.width <- CANVAS_WIDTH
    canvas.height <- CANVAS_HEIGHT
    canvas.style.background <- "black"

    let left = document.getElementById "left"
    let right = document.getElementById "right"

    let buttonWidth = sprintf "%fpx" (CANVAS_WIDTH / 3.)

    left.style.width <- buttonWidth
    right.style.width <- buttonWidth

    left.addEventListener_touchstart (fun e ->
        e.preventDefault ()
        Left |> Some |> Move |> dispatch)
    left.addEventListener_touchend (fun e ->
        e.preventDefault ()
        None |> Move |> dispatch)

    right.addEventListener_touchstart (fun e ->
        e.preventDefault ()
        Right |> Some |> Move |> dispatch)
    right.addEventListener_touchend (fun e ->
        e.preventDefault ()
        None |> Move |> dispatch)

[<Emit("$0 in $1")>]
let checkIn (listener: string) (o: obj) : bool = jsNative

if not (checkIn "ontouchstart" Browser.window) then
    Browser.window.alert "Sorry, game is only for mobile!"
else
    Canvas.Start("canvas", init(), Tick, update, view, subscribe)

