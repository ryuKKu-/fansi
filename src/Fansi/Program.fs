open System
open Elmish
open Fansi
open Fansi.Core
open Fansi.Elmish

type ActivePanel =
    | TimerPanel
    | InputPanel
    | ListPanel

type AppMessage =
    | TimerMsg of TimerComponent.Message
    | TextInputMsg of TextInputComponent.Message
    | ListMsg of ListComponent.Message
    | CheckboxMsg of CheckboxComponent.Message
    | SpinnerMsg of SpinnerComponent.Message
    | SwitchPanel of ActivePanel
    | Test

type Model =
    { Timer: TimerComponent.Model
      TextInput: TextInputComponent.Model
      ItemList: ListComponent.Model<string>
      Checkbox: CheckboxComponent.Model
      Spinner: SpinnerComponent.Model
      Progress: ProgressBarComponent.Model
      ActivePanel: ActivePanel
      Counter: int }

let init () =
    let timer, timerCmd = TimerComponent.init 1 1000.0 (TimeSpan.FromMinutes 1.5)
    let textInput, inputCmd = TextInputComponent.init ()

    let items =
        [ "Compile project"
          "Run tests"
          "Deploy to staging"
          "Review PR"
          "Write docs" ]

    let progress = ProgressBarComponent.init 30 |> ProgressBarComponent.setProgress 0.42

    { Timer = timer
      TextInput = { textInput with Focused = true }
      ItemList =
        { fst (ListComponent.init items id 5) with
            Focused = true }
      Checkbox = fst (CheckboxComponent.init "Enable notifications")
      Spinner = fst (SpinnerComponent.init SpinnerComponent.Dots 80.0 "Loading...")
      Progress = progress
      ActivePanel = InputPanel
      Counter = 0 },
    Cmd.batch
        [ Cmd.mapAppMsg TimerMsg timerCmd
          Cmd.mapAppMsg TextInputMsg inputCmd
          Cmd.mapAppMsg TextInputMsg (Cmd.ofMsg TextInputComponent.Focus) ]

let rec update (msg: FansiMsg<AppMessage>) model =
    match msg with
    | KeyPress cki ->
        match cki.Key with
        | ConsoleKey.Tab ->
            let next =
                match model.ActivePanel with
                | TimerPanel -> InputPanel
                | InputPanel -> ListPanel
                | ListPanel -> TimerPanel

            let m, cmd = update (FansiMsg.App(SwitchPanel next)) model
            m, cmd
        | ConsoleKey.Spacebar when model.ActivePanel = TimerPanel ->
            let m, cmd =
                TimerComponent.update (TimerComponent.StartStopMsg model.Timer.Id) model.Timer

            { model with Timer = m }, Cmd.mapAppMsg TimerMsg cmd
        | ConsoleKey.Spacebar when model.ActivePanel = ListPanel ->
            let m, cmd = CheckboxComponent.update CheckboxComponent.Toggle model.Checkbox
            { model with Checkbox = m }, Cmd.mapAppMsg CheckboxMsg cmd
        | _ ->
            match model.ActivePanel with
            | InputPanel ->
                let m, cmd =
                    TextInputComponent.update (TextInputComponent.KeyInput cki) model.TextInput

                { model with TextInput = m }, Cmd.mapAppMsg TextInputMsg cmd
            | ListPanel ->
                let m, cmd = ListComponent.update (ListComponent.KeyInput cki) model.ItemList
                { model with ItemList = m }, Cmd.mapAppMsg ListMsg cmd
            | _ -> model, Cmd.none

    | App appMessage ->
        match appMessage with
        | TimerMsg tmsg ->
            let m, cmd = TimerComponent.update tmsg model.Timer
            { model with Timer = m }, Cmd.mapAppMsg TimerMsg cmd

        | TextInputMsg imsg ->
            let m, cmd = TextInputComponent.update imsg model.TextInput
            { model with TextInput = m }, Cmd.mapAppMsg TextInputMsg cmd

        | ListMsg lmsg ->
            let m, cmd = ListComponent.update lmsg model.ItemList
            { model with ItemList = m }, Cmd.mapAppMsg ListMsg cmd

        | CheckboxMsg cmsg ->
            let m, cmd = CheckboxComponent.update cmsg model.Checkbox
            { model with Checkbox = m }, Cmd.mapAppMsg CheckboxMsg cmd

        | SpinnerMsg smsg ->
            let m, cmd = SpinnerComponent.update smsg model.Spinner
            { model with Spinner = m }, Cmd.mapAppMsg SpinnerMsg cmd

        | SwitchPanel panel ->
            let textInput =
                { model.TextInput with
                    Focused = (panel = InputPanel) }

            let itemList =
                { model.ItemList with
                    Focused = (panel = ListPanel) }

            { model with
                ActivePanel = panel
                TextInput = textInput
                ItemList = itemList },
            Cmd.none

        | Test ->
            { model with
                Counter = model.Counter + 1 },
            Cmd.none

    | _ -> model, Cmd.none


let view model _ =
    let panelBorder panel =
        if model.ActivePanel = panel then Rounded else Single

    let panelTitleStyle panel =
        if model.ActivePanel = panel then
            { Style.Default with
                FgColor = Color.Cyan
                Bold = true }
        else
            { Style.Default with
                FgColor = Color.BrightBlack }

    let panel border padding children =
        Node.column children
        |> Node.withBorder border
        |> Node.withPadding padding

    let panelPadding = { Edges.Zero with Left = 1; Right = 1 }

    let timerPanel =
        panel (panelBorder TimerPanel) panelPadding
            [ Node.styledText (panelTitleStyle TimerPanel) "Timer"
              TimerComponent.view model.Timer
              Node.text ""
              SpinnerComponent.view model.Spinner
              Node.text ""
              ProgressBarComponent.view model.Progress ]

    let inputPanel =
        panel (panelBorder InputPanel) panelPadding
            [ Node.styledText (panelTitleStyle InputPanel) "Text Input"
              TextInputComponent.view model.TextInput ]

    let listPanel =
        panel (panelBorder ListPanel) panelPadding
            [ Node.styledText (panelTitleStyle ListPanel) "Task List"
              ListComponent.view model.ItemList
              Node.text ""
              CheckboxComponent.view model.Checkbox ]

    let top =
        Node.splitColumns [ timerPanel; inputPanel; listPanel ]
        |> Node.withBorder BorderStyle.Heavy
        |> Node.withWidth (Size.Percent(100))

    let helpText =
        Node.styledText
            { Style.Default with
                FgColor = Color.BrightBlack
                Italic = true }
            "Tab: switch panels | Space: toggle timer/checkbox | Ctrl+C: quit"

    Node.splitRowsRatio [
        (10, top)
        (1, helpText)
    ]


let subscribe model =
    Sub.batch
        [ Sub.map "timer" (FansiMsg.App << TimerMsg) (TimerComponent.subscribe model.Timer)
          Sub.map "textinput" (FansiMsg.App << TextInputMsg) (TextInputComponent.subscribe model.TextInput)
          Sub.map "spinner" (FansiMsg.App << SpinnerMsg) (SpinnerComponent.subscribe model.Spinner) ]

[<EntryPoint>]
let main _ =
    FansiProgram.mkProgram init update view
    |> FansiProgram.withSubscription subscribe
    |> FansiProgram.withFps 60<FPS>
    |> FansiProgram.run

    0
