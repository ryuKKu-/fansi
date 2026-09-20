namespace Fansi

open Elmish
open System
open Fansi
open Fansi.Core
open Fansi.Renderer
open System.Threading.Tasks

type private InternalProgram<'model, 'msg>
    (renderer: Renderer, program: Program<unit, 'model, FansiMsg<'msg>, Node>, mouseEnabled: bool) =
    let mutable dispatch = ignore<FansiMsg<'msg>>
    let mutable oldModel: 'model option = None
    let mutable viewTree = Ui.empty
    let mutable p = Unchecked.defaultof<Program<unit, 'model, FansiMsg<'msg>, Node>>
    let mutable quit = false
    let mutable runProgramLoop = fun () -> ()

    let termWidth () = Console.WindowWidth
    let termHeight () = Console.WindowHeight

    let renderView (tree: Node) =
        renderer.SetFrame(Paint.render (termWidth ()) (termHeight ()) tree)

    let mutable setState =
        fun model dispatch ->
            viewTree <- Program.view p model dispatch
            oldModel <- Some model

    let mutable equal = fun m1 m2 -> obj.ReferenceEquals(m1, m2)

    let shouldRender oldModel newModel = not <| equal oldModel newModel

    let mutable mouseBuffer = System.Text.StringBuilder()

    let tryReadMouseSequence () =
        if mouseBuffer.Length > 0 then
            let buf = mouseBuffer.ToString()

            match Input.tryParseMouseEvent buf with
            | Some evt ->
                mouseBuffer.Clear() |> ignore
                dispatch (MouseEvent evt)
            | None ->
                if mouseBuffer.Length > 20 then
                    mouseBuffer.Clear() |> ignore

    let userInputTask () =
        task {
            while not quit do
                if Console.KeyAvailable then
                    if mouseEnabled then
                        let ch = char (Console.In.Read())

                        if ch = '\x1b' then
                            mouseBuffer.Clear() |> ignore
                            mouseBuffer.Append(ch) |> ignore
                        elif mouseBuffer.Length > 0 then
                            mouseBuffer.Append(ch) |> ignore

                            if ch = 'M' || ch = 'm' then
                                tryReadMouseSequence ()
                        else
                            let cki = Console.ReadKey(true)
                            dispatch (KeyPress cki)
                    else
                        let cki = Console.ReadKey(true)
                        dispatch (KeyPress cki)
                else
                    do! Task.Delay(16)
        }

    member this.Run() =
        task {
            let setDispatch d = dispatch <- d

            p <-
                program
                |> Program.withTermination
                    (fun msg ->
                        match msg with
                        | Quit -> true
                        | _ -> false)
                    (fun _ -> ())
                |> Program.map
                    (fun init arg ->
                        let model, cmd = init arg
                        model, setDispatch :: cmd)
                    id
                    id
                    (fun _ model dispatch -> setState model dispatch)
                    id
                    (fun (predicate, terminate) ->
                        predicate,
                        (fun m ->
                            terminate m
                            quit <- true))

            runProgramLoop <- Program'.runFirstRender () p

            setState <-
                fun model dispatch ->
                    match oldModel with
                    | Some old when shouldRender old model ->
                        viewTree <- Program.view p model dispatch
                        oldModel <- Some model
                        renderView viewTree
                    | None ->
                        viewTree <- Program.view p model dispatch
                        oldModel <- Some model
                        renderView viewTree
                    | _ -> ()

            Console.CancelKeyPress.Add(fun args ->
                args.Cancel <- true
                dispatch Quit
                quit <- true)

            runProgramLoop ()
            renderView viewTree

            do! userInputTask ()

            renderer.Stop()
        }

[<RequireQualifiedAccess>]
module FansiProgram =

    type FansiProgram<'model, 'msg> =
        private
            { program: Program<unit, 'model, FansiMsg<'msg>, Node>
              fps: int<FPS>
              mouseEnabled: bool }

    let mkProgram
        (init: unit -> 'model * Cmd<FansiMsg<'msg>>)
        (update: FansiMsg<'msg> -> 'model -> 'model * Cmd<FansiMsg<'msg>>)
        (view: 'model -> Dispatch<FansiMsg<'msg>> -> Node)
        =
        { program = Program.mkProgram init update view
          fps = 60<FPS>
          mouseEnabled = false }

    let withFps fps (p: FansiProgram<'model, 'msg>) = { p with fps = fps }

    let withMouseEnabled (p: FansiProgram<'model, 'msg>) = { p with mouseEnabled = true }

    let withSubscription subscribe (p: FansiProgram<'model, 'msg>) =
        { p with
            program = Program.withSubscription subscribe p.program }

    let run (p: FansiProgram<'model, 'msg>) =
        let renderer = Renderer(p.fps)

        renderer.Execute(AnsiSequence.enableAltScreenBuffer)
        renderer.Execute(AnsiSequence.hideCursor)

        if p.mouseEnabled then
            renderer.Execute(AnsiSequence.enableMouseTracking)

        renderer.Start()

        let program = InternalProgram(renderer, p.program, p.mouseEnabled)

        try
            do program.Run() |> Async.AwaitTask |> Async.RunSynchronously
        finally
            if p.mouseEnabled then
                renderer.Execute(AnsiSequence.disableMouseTracking)

            renderer.Execute(AnsiSequence.showCursor)
            renderer.Execute(AnsiSequence.disableAltScreenBuffer)
