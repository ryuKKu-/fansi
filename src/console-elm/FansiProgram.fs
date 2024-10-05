namespace Fansi

open Elmish
open System
open Fansi.Renderer
open System.Threading.Tasks

type private InternalProgram<'model, 'msg>(renderer: Renderer, program: Program<unit, 'model, 'msg, string>, keyPressHandler: ConsoleKeyInfo -> Dispatch<'msg> -> unit, cancelSignalMsg: 'msg) =
    let mutable dispatch = ignore<'msg>
    let mutable oldModel: 'model option = None
    let mutable view = Unchecked.defaultof<_>
    let mutable p = Unchecked.defaultof<Program<unit, 'model, 'msg, string>>
    let mutable quit = false
    let mutable runProgramLoop = fun () -> ()
            
    let mutable setState = fun model dispatch ->
        view <- Program.view p model dispatch
        oldModel <- Some model
    
    let userInputTask () = task {
        while not quit do
            if Console.KeyAvailable then
                let cki = Console.ReadKey(true)
                keyPressHandler cki dispatch
                do! Task.Delay(100) 
    }
    
    member val Equal = (fun m1 m2 -> obj.ReferenceEquals(m1, m2)) with get, set
    
    member this.ShouldRender(oldModel, newModel) =
        not <| this.Equal oldModel newModel
        
    member this.Run() = task {
        let setDispatch d =
            dispatch <- d
        
        p <- program
            |> Program.withConsoleTrace
            |> Program.map
                (fun init arg ->
                    let model, cmd = init arg
                    model, setDispatch :: cmd)
                id
                id
                (fun _ model dispatch -> setState model dispatch)
                id
                (fun (predicate, terminate) -> predicate, (fun m -> terminate(m); quit <- true))

        runProgramLoop <- Program'.runFirstRender () p
        
        setState <- fun model dispatch ->
            match oldModel with
            | Some old when this.ShouldRender(old, model) ->
                view <- Program.view p model dispatch
                oldModel <- Some model
                renderer.Write view
            | _ -> ()

        Console.CancelKeyPress.Add(fun args ->
            if not(obj.ReferenceEquals(cancelSignalMsg, null)) then
                args.Cancel <- true
                dispatch cancelSignalMsg
                quit <- true)
                
        runProgramLoop()
        
        renderer.Write view

        do! userInputTask()
        
        renderer.Stop();
    }

[<RequireQualifiedAccess>]
module FansiProgram = 

    type FansiProgram<'model, 'msg> = private {
        program: Program<unit, 'model, 'msg, string>
        fps: int<FPS>
        keyPressHandler: ConsoleKeyInfo -> Dispatch<'msg> -> unit
        cancelSignalMessage: 'msg
    }
    
    let mkProgram
        (init : unit -> 'model * Cmd<'msg>)
        (update : 'msg -> 'model -> 'model * Cmd<'msg>)
        (view : 'model -> Dispatch<'msg> -> string) =
        {
            program = Program.mkProgram init update view
            fps = 60<FPS>
            keyPressHandler = fun _ _ -> ()
            cancelSignalMessage = Unchecked.defaultof<'msg> 
        }
    
    let withFps fps (p: FansiProgram<'model, 'msg>) = { p with fps = fps }
    
    let withKeyPressHandler keyPressHandler (p: FansiProgram<'model, 'msg>) = { p with keyPressHandler = keyPressHandler }
    
    let withTermination predicate termination (p: FansiProgram<'model, 'msg>) = { p with program = Program.withTermination predicate termination p.program }
    
    let withSubscription subscribe (p: FansiProgram<'model, 'msg>) = { p with program = Program.withSubscription subscribe p.program }
    
    let withCancelSignalMessage msg (p: FansiProgram<'model, 'msg>) = { p with cancelSignalMessage = msg }
    
    let run (p: FansiProgram<'model, 'msg>) =
        let renderer = Renderer(p.fps)
        renderer.Start()
        
        let program = InternalProgram(renderer, p.program, p.keyPressHandler, p.cancelSignalMessage)
    
        do program.Run()
            |> Async.AwaitTask
            |> Async.RunSynchronously
            