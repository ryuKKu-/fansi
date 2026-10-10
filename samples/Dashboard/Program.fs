module Fansi.Samples.Dashboard

open System
open Elmish
open Fansi
open Fansi.Core

type Panel =
    | Clock
    | Input
    | Tasks

type Msg =
    | InputMsg of TextInputComponent.Message
    | TimerMsg of TimerComponent.Message
    | SpinnerMsg of SpinnerComponent.Message

type Model =
    { Timer: TimerComponent.Model
      Spinner: SpinnerComponent.Model
      Progress: ProgressBarComponent.Model
      Input: TextInputComponent.Model
      Tasks: ListComponent.Model<string>
      KeepText: CheckboxComponent.Model
      Focus: Focus<Panel> }

let private countdown = TimeSpan.FromMinutes 1.0

let init () =
    let timer, _ = TimerComponent.init 100<ms> countdown
    let spinner, _ = SpinnerComponent.init SpinnerComponent.Dots 80<ms> "running"
    let input, _ = TextInputComponent.init ()

    let tasks = ListComponent.init [ "layout sample"; "dashboard"; "dead helpers" ] id 5

    let keepText, _ = CheckboxComponent.init "keep the text after adding"

    { Timer = timer
      Spinner = spinner
      // 10 cells plus " 100%" fits the clock panel at 80 columns.
      Progress = ProgressBarComponent.init 10
      Input =
        { input with
            Width = 24
            Placeholder = "a new task" }
      Tasks = tasks
      KeepText = keepText
      Focus = Focus.ofList [ Clock; Input; Tasks ] },
    Cmd.none

let private updateInput msg model =
    let input, cmd = TextInputComponent.update msg model.Input
    { model with Input = input }, Cmd.map InputMsg cmd

let private addTask model =
    let task = model.Input.Value.Trim()

    if task = "" then
        model, Cmd.none
    else
        let items = model.Tasks.Items @ [ task ]

        let model =
            { model with
                Tasks =
                    { model.Tasks with
                        Items = items
                        FocusItemIndex = items.Length - 1
                        ViewportOffset = max 0 (items.Length - model.Tasks.ViewportSize) } }

        if model.KeepText.Checked then
            model, Cmd.none
        else
            updateInput (TextInputComponent.SetValue "") model

let private onKey (k: KeyEvent) model =
    match k.Key, Focus.current model.Focus with
    | Key.Esc, _ -> model, Cmd.quit
    | Key.Tab, _ when k.Shift ->
        { model with
            Focus = Focus.prev model.Focus },
        Cmd.none
    | Key.Tab, _ ->
        { model with
            Focus = Focus.next model.Focus },
        Cmd.none
    | Key.Char ' ', Clock when model.Timer.Timeout <= TimeSpan.Zero ->
        { model with
            Timer =
                { model.Timer with
                    Timeout = countdown
                    Running = true }
            Progress = ProgressBarComponent.setProgress 0.0 model.Progress },
        Cmd.none
    | Key.Char ' ', Clock -> model, Cmd.map TimerMsg (model.Timer.Toggle())
    | _, Clock -> model, Cmd.none
    | Key.Enter, Input -> addTask model
    | _, Input -> updateInput (TextInputComponent.KeyInput k) model
    | Key.Char ' ', Tasks ->
        let keepText, _ = CheckboxComponent.update CheckboxComponent.Toggle model.KeepText
        { model with KeepText = keepText }, Cmd.none
    | _, Tasks ->
        let tasks, _ = ListComponent.update (ListComponent.KeyInput k) model.Tasks
        { model with Tasks = tasks }, Cmd.none

let update (msg: FansiMsg<Msg>) (model: Model) =
    match msg with
    | KeyPress k -> onKey k model
    | Paste text when Focus.isFocused Input model.Focus -> updateInput (TextInputComponent.Pasted text) model
    | App(InputMsg m) -> updateInput m model
    | App(TimerMsg m) ->
        let timer, cmd = TimerComponent.update m model.Timer
        let elapsed = 1.0 - timer.Timeout.TotalMilliseconds / countdown.TotalMilliseconds

        { model with
            Timer = timer
            Progress = ProgressBarComponent.setProgress elapsed model.Progress },
        Cmd.map TimerMsg cmd
    | App(SpinnerMsg m) ->
        let spinner, _ = SpinnerComponent.update m model.Spinner
        { model with Spinner = spinner }, Cmd.none
    | Paste _
    | Mouse _
    | Resize _
    | FocusChanged _ -> model, Cmd.none

let subscribe model =
    Sub.batch
        [ TimerComponent.subscribe model.Timer |> Sub.map TimerMsg
          SpinnerComponent.subscribe model.Spinner |> Sub.map SpinnerMsg
          TextInputComponent.subscribe (Focus.isFocused Input model.Focus) model.Input
          |> Sub.map InputMsg ]

let private panel title focused body =
    Ui.col body
    |> Ui.border (if focused then Rounded else Single)
    |> Ui.titleWith (
        Ui.text title
        |> Ui.bold
        |> Ui.fg (if focused then Color.Cyan else Color.BrightBlack)
    )
    |> Ui.padX 1

let view model =
    let on panel = Focus.isFocused panel model.Focus

    let clock =
        panel
            "Clock"
            (on Clock)
            [ TimerComponent.view model.Timer
              SpinnerComponent.view model.Spinner
              Ui.text ""
              ProgressBarComponent.view model.Progress ]
        |> Ui.pct 25

    let input =
        panel
            "New task"
            (on Input)
            [ TextInputComponent.view (on Input) model.Input |> Ui.border Single |> Ui.len 3
              Ui.text "enter adds it to the list" |> Ui.fg Color.BrightBlack ]
        |> Ui.fill 1

    let tasks =
        panel
            "Tasks"
            (on Tasks)
            [ ListComponent.view model.Tasks
              Ui.text ""
              CheckboxComponent.view model.KeepText ]
        |> Ui.ratio 1 3

    let selected =
        match ListComponent.selectedItem model.Tasks with
        | Some task -> $"selected: {task}"
        | None -> "nothing selected"

    Ui.col
        [ Ui.row [ clock; input; tasks ] |> Ui.border Heavy |> Ui.fill 1
          Ui.text selected |> Ui.len 1
          Ui.text "tab: panel - space: pause / tick box - enter: add / select - esc: quit"
          |> Ui.fg Color.BrightBlack
          |> Ui.italic
          |> Ui.len 1 ]

[<EntryPoint>]
let main _ =
    FansiProgram.mkProgram init update view
    |> FansiProgram.withSubscription subscribe
    |> FansiProgram.run

    0
