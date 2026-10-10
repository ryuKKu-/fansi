module Fansi.Samples.Components

open System
open Elmish
open Fansi
open Fansi.Core

type Field =
    | Name
    | Secret
    | Fruits
    | Help
    | Agree
    | Submit

type Msg =
    | NameMsg of TextInputComponent.Message
    | SecretMsg of TextInputComponent.Message
    | TimerMsg of TimerComponent.Message
    | SpinnerMsg of SpinnerComponent.Message

type Model =
    { Name: TextInputComponent.Model
      Secret: TextInputComponent.Model
      Fruits: ListComponent.Model<string>
      Help: ViewportComponent.Model
      Agree: CheckboxComponent.Model
      Submit: ButtonComponent.Model
      Timer: TimerComponent.Model
      Spinner: SpinnerComponent.Model
      Progress: ProgressBarComponent.Model
      Focus: Focus<Field>
      Status: string }

let private countdown = TimeSpan.FromSeconds 30.0

let private required s =
    if String.IsNullOrWhiteSpace s then
        TextInputComponent.Invalid "a name is required"
    else
        TextInputComponent.Valid

let private helpText =
    [ "Tab and Shift+Tab move focus between the boxes."
      "Up and Down scroll this box one row at a time."
      "PageUp and PageDown, or Space, scroll a page."
      "Ctrl+U and Ctrl+D scroll half a page."
      "Home and End jump to the top and the bottom."
      "The mouse wheel scrolls this box too."
      ""
      "Long lines wrap to the width of the box and never split a character: 日本語 and 👋 move whole."
      ""
      "The title shows how far down you are." ]

let init () =
    let name, _ = TextInputComponent.init ()
    let secret, _ = TextInputComponent.init ()

    let fruits =
        ListComponent.init [ "apple"; "banana"; "cherry"; "damson"; "elderberry"; "fig" ] id 3

    // Loaded through init, not setText. Empty content counts as the bottom, and
    // setText would then follow the end of the text.
    let help, _ = ViewportComponent.init 36 3 (helpText |> List.map Ui.text)

    let agree, _ = CheckboxComponent.init "I have read the checklist"
    let submit, _ = ButtonComponent.init "Submit"
    let timer, _ = TimerComponent.init 100<ms> countdown
    let spinner, _ = SpinnerComponent.init SpinnerComponent.Dots 80<ms> "working"

    { Name =
        { name with
            Width = 20
            Placeholder = "your name"
            Suggestions = [ "Ada Lovelace"; "Alan Turing"; "Barbara Liskov"; "Grace Hopper" ] }
        |> TextInputComponent.withValidation required
      Secret =
        { secret with
            Echo = TextInputComponent.Password '*'
            CharLimit = 16
            Placeholder = "password"
            Suggestions = [ "secret-password" ] }
      Fruits = fruits
      Help = help
      Agree = agree
      Submit = submit
      Timer = timer
      Spinner = spinner
      Progress = ProgressBarComponent.init 20
      Focus = Focus.ofList [ Name; Secret; Fruits; Help; Agree; Submit ]
      Status = "" },
    Cmd.none

let private updateName msg model =
    let name, cmd = TextInputComponent.update msg model.Name
    { model with Name = name }, Cmd.map NameMsg cmd

let private updateSecret msg model =
    let secret, cmd = TextInputComponent.update msg model.Secret
    { model with Secret = secret }, Cmd.map SecretMsg cmd

let private onKey (k: KeyEvent) model =
    let focused = Focus.current model.Focus

    match k.Key, focused with
    | Key.Esc, _ -> model, Cmd.quit
    | Key.Tab, _ when k.Shift ->
        { model with
            Focus = Focus.prev model.Focus },
        Cmd.none
    // Tab completes a suggestion when one is showing, and moves focus otherwise.
    // The parent decides, because only it knows both jobs exist.
    | Key.Tab, Name when (TextInputComponent.currentSuggestion model.Name).IsSome ->
        updateName (TextInputComponent.KeyInput k) model
    | Key.Tab, _ ->
        { model with
            Focus = Focus.next model.Focus },
        Cmd.none
    | Key.Char 's', _ when k.Ctrl -> model, Cmd.map TimerMsg (model.Timer.Toggle())
    | _, Name -> updateName (TextInputComponent.KeyInput k) model
    | _, Secret -> updateSecret (TextInputComponent.KeyInput k) model
    | _, Fruits ->
        let fruits, _ = ListComponent.update (ListComponent.KeyInput k) model.Fruits
        { model with Fruits = fruits }, Cmd.none
    | _, Help ->
        let help, _ = ViewportComponent.update (ViewportComponent.KeyInput k) model.Help
        { model with Help = help }, Cmd.none
    | (Key.Enter | Key.Char ' '), Agree ->
        let agree, _ = CheckboxComponent.update CheckboxComponent.Toggle model.Agree
        { model with Agree = agree }, Cmd.none
    | Key.Enter, Submit ->
        let fruit =
            ListComponent.selectedItem model.Fruits |> Option.defaultValue "no fruit"

        { model with
            Status =
                $"submitted: name={model.Name.Value}, password length={model.Secret.Value.Length}, {fruit}, agreed={model.Agree.Checked}" },
        Cmd.none
    | _ -> model, Cmd.none

let update msg model =
    match msg with
    | KeyPress k -> onKey k model
    | Paste text ->
        match Focus.current model.Focus with
        | Name -> updateName (TextInputComponent.Pasted text) model
        | Secret -> updateSecret (TextInputComponent.Pasted text) model
        | _ -> model, Cmd.none
    | App(NameMsg m) -> updateName m model
    | App(SecretMsg m) -> updateSecret m model
    | App(TimerMsg(TimerComponent.TimedOutMsg _)) -> { model with Status = "time is up" }, Cmd.none
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
    // The help box is the only thing that scrolls, so the wheel always goes to it.
    | Mouse m ->
        let help, _ = ViewportComponent.update (ViewportComponent.MouseInput m) model.Help
        { model with Help = help }, Cmd.none
    // Half the screen, less a border and one cell of padding on each side.
    | Resize(w, _) ->
        { model with
            Help = ViewportComponent.setSize (max 1 (w / 2 - 4)) 3 model.Help },
        Cmd.none
    | FocusChanged _ -> model, Cmd.none

let subscribe model =
    Sub.batch
        [ TextInputComponent.subscribe (Focus.isFocused Name model.Focus) model.Name
          |> Sub.map NameMsg
          TextInputComponent.subscribe (Focus.isFocused Secret model.Focus) model.Secret
          |> Sub.map SecretMsg
          TimerComponent.subscribe model.Timer |> Sub.map TimerMsg
          SpinnerComponent.subscribe model.Spinner |> Sub.map SpinnerMsg ]

let private framed focused node =
    node |> Ui.border (if focused then Rounded else Single) |> Ui.padX 1

let view model =
    let on field = Focus.isFocused field model.Focus

    let error =
        match model.Name.Error with
        | Some e -> Ui.text e |> Ui.fg Color.Red
        | None -> Ui.text ""

    Ui.col
        [ Ui.text "Components" |> Ui.bold |> Ui.fg Color.Cyan |> Ui.len 1
          TextInputComponent.view (on Name) model.Name |> framed (on Name) |> Ui.len 3
          error |> Ui.padX 1 |> Ui.len 1
          TextInputComponent.view (on Secret) model.Secret
          |> framed (on Secret)
          |> Ui.len 3
          Ui.row
              [ ListComponent.view model.Fruits |> framed (on Fruits) |> Ui.fill 1
                ViewportComponent.view model.Help
                |> framed (on Help)
                |> Ui.title $"Help {ViewportComponent.scrollPercent model.Help}%%"
                |> Ui.fill 1 ]
          |> Ui.len 5
          CheckboxComponent.view model.Agree |> framed (on Agree) |> Ui.len 3
          ButtonComponent.view (on Submit) model.Submit |> Ui.len 3
          Ui.row
              [ SpinnerComponent.view model.Spinner
                Ui.text "   "
                TimerComponent.view model.Timer
                Ui.text "   "
                ProgressBarComponent.view model.Progress ]
          |> Ui.len 1
          Ui.text model.Status |> Ui.len 1
          Ui.empty |> Ui.fill 1
          Ui.text "tab / shift+tab move - tab completes a name - ctrl+s pauses timer - esc quits"
          |> Ui.fg Color.BrightBlack
          |> Ui.italic
          |> Ui.len 1 ]

[<EntryPoint>]
let main _ =
    FansiProgram.mkProgram init update view
    |> FansiProgram.withSubscription subscribe
    |> FansiProgram.run

    0
