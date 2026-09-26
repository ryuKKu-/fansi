namespace Fansi

open System
open System.Text
open System.Threading
open Elmish
open Fansi
open Fansi.Core
open Fansi.Keymap

[<RequireQualifiedAccess>]
module VirtualCursor =
    let mutable lastId = 0L

    let nextId () = Interlocked.Increment(&lastId)

    type CursorId = int64

    type CursorType =
        | Blink
        | Static
        | Hidden

    type Message =
        | Focus of CursorId
        | Blur of CursorId
        | BlinkTick of CursorId

    type Model =
        { Id: CursorId
          Type: CursorType
          BlinkSpeed: float
          Focused: bool
          Char: char
          Blink: bool }

    /// A function, not a value: each call must claim its own id, or every text input
    /// on screen would answer to the same Focus/Blur and share one blink timer.
    let create () =
        { Id = nextId ()
          Type = Blink
          Char = ' '
          Focused = false
          Blink = true
          BlinkSpeed = 530.0 }

    let setCharacter c (model: Model) = { model with Char = c }

    let update msg model =
        match msg with
        | Focus id ->
            if model.Id <> id then
                model, Cmd.none
            else
                { model with
                    Focused = true
                    Blink = model.Type <> Hidden },
                Cmd.none
        | Blur id ->
            if model.Id <> id then
                model, Cmd.none
            else
                { model with
                    Focused = false
                    Blink = false },
                Cmd.none
        | BlinkTick id ->
            if model.Id <> id || not model.Focused || model.Type <> Blink then
                model, Cmd.none
            else
                { model with Blink = not model.Blink }, Cmd.none

    let view model : Node =
        if model.Blink then
            Ui.text (string model.Char) |> Ui.bg Color.Cyan
        else
            Ui.text (string model.Char)

    let subscribe model =
        [ if model.Focused && model.Type = Blink then
              [ "cursor"; string model.Id ], Sub.timer model.BlinkSpeed (BlinkTick model.Id) ]


[<RequireQualifiedAccess>]
module TextInputComponent =

    type Message =
        | Focus
        | Blur
        | CursorMsg of VirtualCursor.Message
        | KeyInput of ConsoleKeyInfo

    type Keymap =
        { CharacterForward: KeyBind
          CharacterBackward: KeyBind
          DeleteBackward: KeyBind
          DeleteForward: KeyBind }

    let defaultKeymap =
        { CharacterForward =
            KeyBind.create
                { Key = ConsoleKey.RightArrow
                  Modifier = None }
          CharacterBackward =
            KeyBind.create
                { Key = ConsoleKey.LeftArrow
                  Modifier = None }
          DeleteBackward =
            KeyBind.create
                { Key = ConsoleKey.Backspace
                  Modifier = None }
          DeleteForward =
            KeyBind.create
                { Key = ConsoleKey.Delete
                  Modifier = None } }

    type Model =
        { Focused: bool
          Value: StringBuilder
          CharLimit: int
          Keymap: Keymap
          Prompt: string
          PromptStyle: Style
          TextStyle: Style
          CursorPosition: int
          UseVirtualCursor: bool
          Cursor: VirtualCursor.Model }

    let setCursorPosition pos (model: Model) =
        let p = Math.Clamp(pos, 0, model.Value.Length)
        let c = if p < model.Value.Length then model.Value.Chars p else ' '

        { model with
            CursorPosition = p
            Cursor.Char = c }

    let insertSpan (s: ReadOnlySpan<char>) (model: Model) =
        // The room left is the limit minus what is already stored. Subtracting the
        // incoming length as well truncated a span that fitted exactly, and went
        // negative when the span was too long, which threw from Slice.
        let span =
            if model.CharLimit <= 0 then
                s
            else
                let room = max 0 (model.CharLimit - model.Value.Length)
                if s.Length <= room then s else s.Slice(0, room)

        model.Value.Insert(model.CursorPosition, span) |> ignore
        setCursorPosition (model.CursorPosition + span.Length) model

    let init () =
        { Focused = false
          Value = StringBuilder()
          Keymap = defaultKeymap
          CharLimit = 0
          Prompt = "> "
          PromptStyle =
            { Style.Default with
                FgColor = Color.BrightCyan
                Bold = true }
          TextStyle = Style.Default
          CursorPosition = 0
          UseVirtualCursor = true
          Cursor = VirtualCursor.create () },
        Cmd.none

    let update msg model =
        match msg with
        | Focus ->
            if not model.UseVirtualCursor then
                { model with Focused = true }, Cmd.none
            else
                let cursor, _ =
                    VirtualCursor.update (VirtualCursor.Focus model.Cursor.Id) model.Cursor

                { model with
                    Focused = true
                    Cursor = cursor },
                Cmd.none

        | Blur ->
            if not model.UseVirtualCursor then
                { model with Focused = false }, Cmd.none
            else
                let cursor, _ =
                    VirtualCursor.update (VirtualCursor.Blur model.Cursor.Id) model.Cursor

                { model with
                    Focused = false
                    Cursor = cursor },
                Cmd.none

        | CursorMsg msg ->
            let m, cmd = VirtualCursor.update msg model.Cursor
            { model with Cursor = m }, Cmd.map CursorMsg cmd

        | KeyInput cki when model.Focused ->
            if
                Keymap.``match`` model.Keymap.CharacterForward cki
                && model.CursorPosition < model.Value.Length
            then
                setCursorPosition (model.CursorPosition + 1) model, Cmd.none
            elif Keymap.``match`` model.Keymap.CharacterBackward cki && model.CursorPosition > 0 then
                setCursorPosition (model.CursorPosition - 1) model, Cmd.none
            elif Keymap.``match`` model.Keymap.DeleteBackward cki then
                if model.CursorPosition > 0 then
                    model.Value.Remove(model.CursorPosition - 1, 1) |> ignore
                    setCursorPosition (model.CursorPosition - 1) model, Cmd.none
                else
                    model, Cmd.none
            elif Keymap.``match`` model.Keymap.DeleteForward cki then
                if model.CursorPosition < model.Value.Length then
                    model.Value.Remove(model.CursorPosition, 1) |> ignore
                    setCursorPosition model.CursorPosition model, Cmd.none
                else
                    model, Cmd.none
            elif not (Char.IsControl cki.KeyChar) then
                model.Value.Insert(model.CursorPosition, cki.KeyChar) |> ignore
                setCursorPosition (model.CursorPosition + 1) model, Cmd.none
            else
                model, Cmd.none

        | _ -> model, Cmd.none

    let subscribe model =
        if model.Focused then
            Sub.map "input" CursorMsg (VirtualCursor.subscribe model.Cursor)
        else
            []

    let view model : Node =
        let value = model.Value.ToString()
        let before = value[.. model.CursorPosition - 1]

        Ui.row
            [ Ui.text model.Prompt |> Ui.style model.PromptStyle
              Ui.text before |> Ui.style model.TextStyle
              VirtualCursor.view model.Cursor
              if model.CursorPosition < value.Length then
                  Ui.text (value.Substring(model.CursorPosition + 1)) |> Ui.style model.TextStyle ]
