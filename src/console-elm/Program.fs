open System
open System.Text
open Elmish
open Fansi
  
open Fansi.Keymap
    
type Message =
    | Tick of DateTimeOffset
    | TimerCompMsg of TimerComponent.Message
    | TextInputCompMsg of TextInputComponent.Message
    | KeyPressMsg of ConsoleKeyInfo
    | Quit

type Model = {
    quitting: bool
    ticks: int64
    timer: TimerComponent.Model
    keymap: KeyBind
    textInput: TextInputComponent.Model
}

let init =
    let timer, cmdT = TimerComponent.init 1 1000 (TimeSpan.FromSeconds(10))
    let textInput, cmd = TextInputComponent.init ()
    let keymap = KeyBind.create [| { Key = ConsoleKey.A; Modifier = None } |]
    
    let model = { ticks = 0
                  timer = timer
                  quitting = false
                  keymap = keymap
                  textInput = textInput }
    
    let cmds = Cmd.batch [
        Cmd.map TimerCompMsg cmdT
        Cmd.map TextInputCompMsg cmd
    ]
    
    model, cmds

let update (msg: Message) (model: Model) =
    match msg with
    | Tick dateTimeOffset ->
        { model with ticks = dateTimeOffset.Ticks }, Cmd.none

    | TimerCompMsg timerMessage ->
        let m, cmd = TimerComponent.update timerMessage model.timer
        { model with timer = m }, Cmd.map TimerCompMsg cmd

    | KeyPressMsg cki ->
        if cki.Key = ConsoleKey.Enter then
            { model with quitting = true }, Cmd.ofMsg Quit
        else if Keymap.``match`` model.keymap cki then
            model, Cmd.map TimerCompMsg (model.timer.Toggle())
        else if cki.Key = ConsoleKey.Tab then
            let t, cmd = TextInputComponent.update TextInputComponent.Focus model.textInput
            { model with textInput = t }, Cmd.map TextInputCompMsg cmd
        else
            model, Cmd.none
    
    | Quit ->
        { model with quitting = true }, Cmd.none

    | TextInputCompMsg msg ->
        let t, cmd = TextInputComponent.update msg model.textInput
        { model with textInput = t }, Cmd.map TextInputCompMsg cmd
        
      
let view (model: Model) _ =
    if not model.quitting then
        let x = StringBuilder()
        x.AppendLine(TimerComponent.view model.timer) |> ignore
        x.AppendLine(TextInputComponent.view model.textInput) |> ignore
        $"\x1b[32m{x.ToString()}\x1b[0m"
    else
        "See you :)"
        
let subscribe model =
    Sub.batch [
        Sub.map "timer" TimerCompMsg (TimerComponent.subscribe model.timer)
        Sub.map "textinput" TextInputCompMsg (TextInputComponent.subscribe model.textInput)
    ]

[<EntryPoint>]
let main _ = 
    FansiProgram.mkProgram (fun _ -> init) update view
        |> FansiProgram.withSubscription subscribe
        |> FansiProgram.withKeyPressHandler (fun cki dispatch -> KeyPressMsg cki |> dispatch)
        |> FansiProgram.withFps 60<FPS>
        |> FansiProgram.withTermination (fun msg -> msg = Quit) (fun _ -> ())
        |> FansiProgram.withCancelSignalMessage(Quit)
        |> FansiProgram.run
    
    0