module Fansi.Samples.Interactive

open Elmish
open Fansi
open Fansi.Core

type Panel =
    | Typing
    | Events

/// The app's own message type. QuitTimerFired is what proves the framework's
/// Cmd -> FansiMsg lift actually delivers a command's message back to update.
type AppMsg = QuitTimerFired

type Model =
    { Typed: string
      LastKey: KeyEvent option
      LastPaste: int option
      Size: int * int
      Log: string list
      Focus: Focus<Panel> }

let private note line model =
    { model with
        Log = (line :: model.Log) |> List.truncate 8 }

let init () =
    { Typed = ""
      LastKey = None
      LastPaste = None
      Size = 0, 0
      Log = [ "ready" ]
      Focus = Focus.ofList [ Typing; Events ] },
    Cmd.none

let update msg model =
    match msg with
    | KeyPress k ->
        let model = { model with LastKey = Some k }

        match k.Key with
        | Key.Esc -> model, Cmd.quit
        | Key.Tab when k.Shift ->
            { model with
                Focus = Focus.prev model.Focus },
            Cmd.none
        | Key.Tab ->
            { model with
                Focus = Focus.next model.Focus },
            Cmd.none
        | Key.Backspace when model.Typed.Length > 0 ->
            { model with
                Typed = model.Typed.Substring(0, model.Typed.Length - 1) },
            Cmd.none
        // Deliberately throws, so a person can check the program dies loudly
        // instead of freezing on its last frame.
        | Key.Char 'e' when k.Ctrl -> failwith "update threw on purpose (ctrl+e)"
        // Arms a quit with no more keys pressed, to prove a delayed command can
        // still stop the program - and that its message makes it back to update
        // through the App lift, not just Cmd.quit called straight from a key.
        | Key.Char 't' when k.Ctrl ->
            model |> note "quitting from a timer in 2s - hands off the keyboard", Cmd.after 2000<ms> QuitTimerFired
        | Key.Char c when not k.Ctrl && not k.Alt ->
            { model with
                Typed = model.Typed + string c },
            Cmd.none
        | other -> model |> note $"key {other} ctrl={k.Ctrl} alt={k.Alt} shift={k.Shift}", Cmd.none

    | Paste text ->
        { model with
            LastPaste = Some text.Length }
        |> note $"pasted {text.Length} characters",
        Cmd.none
    | Mouse m -> model |> note $"mouse {m.Button} {m.Action} at {m.X},{m.Y}", Cmd.none
    | Resize(w, h) -> { model with Size = w, h } |> note $"resized to {w}x{h}", Cmd.none
    | FocusChanged f -> model |> note $"terminal focus {f}", Cmd.none
    | App QuitTimerFired -> model |> note "timer fired - quitting", Cmd.quit

let private panel title focused body =
    Ui.col
        [ Ui.text title
          |> Ui.bold
          |> Ui.fg (if focused then Color.Cyan else Color.BrightBlack)
          Ui.text ""
          body ]
    |> Ui.border (if focused then Rounded else Single)
    |> Ui.padX 1
    |> Ui.fill 1

let view model =
    let w, h = model.Size

    let lastKey =
        match model.LastKey with
        | Some k -> $"{k.Key} ctrl={k.Ctrl} alt={k.Alt} shift={k.Shift}"
        | None -> "(none yet)"

    let lastPaste =
        match model.LastPaste with
        | Some n -> $"{n} characters"
        | None -> "(none yet)"

    Ui.col
        [ Ui.row
              [ panel "Typing" (Focus.isFocused Typing model.Focus) (Ui.text $"> {model.Typed}")
                panel "Events" (Focus.isFocused Events model.Focus) (Ui.text (String.concat "\n" model.Log)) ]
          |> Ui.fill 1

          Ui.text $"size {w}x{h} - last key: {lastKey} - last paste: {lastPaste}"
          |> Ui.fg Color.BrightBlack
          |> Ui.len 1

          Ui.text
              "tab / shift+tab switch panels - esc quits - ctrl+c quits - ctrl+t quits from a timer - ctrl+e throws on purpose"
          |> Ui.fg Color.BrightBlack
          |> Ui.italic
          |> Ui.len 1 ]

[<EntryPoint>]
let main _ =
    FansiProgram.mkProgram init update view
    |> FansiProgram.withMouseEnabled
    |> FansiProgram.run

    0
