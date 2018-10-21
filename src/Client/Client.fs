module Client

open System

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Canvas
open Physics.Consts

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

let update (model: Model) = function
    | Tick delta ->
        Physics.update model.Engine delta
        model

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

    renderShape ctx !^"yellow" model.Player

    ctx.restore()

let subscribe (canvas: Browser.HTMLCanvasElement) dispatch (model : Model) =
    canvas.width <- CANVAS_WIDTH
    canvas.height <- CANVAS_HEIGHT
    canvas.style.background <- "black"

Canvas.Start("canvas", init(), Tick, update, view, subscribe)