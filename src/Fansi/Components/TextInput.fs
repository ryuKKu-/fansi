namespace Fansi

open System
open System.Text
open Elmish
open Fansi.Core
open Fansi.Keymap

[<RequireQualifiedAccess>]
module Cursor =
    type CursorId = int

    type CursorType =
        | Blink
        | Static
        | Hidden

    type Message =
        | Focus of CursorId
        | BlinkTick of CursorId

    type Model =
        { Id: CursorId
          Type: CursorType
          BlinkSpeed: float
          Focused: bool
          Char: char
          Blink: bool }

    let init id =
        { Id = id
          Type = Blink
          Char = '│'
          Focused = false
          Blink = true
          BlinkSpeed = 530.0 }

    let update msg model =
        match msg with
        | Focus id ->
            if model.Id <> id then model, Cmd.none
            else
                { model with Focused = true; Blink = model.Type <> Hidden }, Cmd.none
        | BlinkTick id ->
            if model.Id <> id || not model.Focused || model.Type <> Blink then
                model, Cmd.none
            else
                { model with Blink = not model.Blink }, Cmd.none

    let view model : Node =
        if model.Blink then
            Node.styledText
                { Style.Default with FgColor = Color.Cyan }
                (string model.Char)
        else
            Node.text " "

    let subscribe model =
        [ if model.Focused && model.Type = Blink then
            [ "cursor"; string model.Id ], Sub.timer model.BlinkSpeed (BlinkTick model.Id) ]


[<RequireQualifiedAccess>]
module TextInputComponent =

    type Message =
        | Focus
        | Unfocus
        | CursorMsg of Cursor.Message
        | KeyInput of ConsoleKeyInfo

    type Keymap =
        { CharacterForward: KeyBind
          CharacterBackward: KeyBind
          DeleteBackward: KeyBind
          DeleteForward: KeyBind }

    let defaultKeymap =
        { CharacterForward = KeyBind.create { Key = ConsoleKey.RightArrow; Modifier = None }
          CharacterBackward = KeyBind.create { Key = ConsoleKey.LeftArrow; Modifier = None }
          DeleteBackward = KeyBind.create { Key = ConsoleKey.Backspace; Modifier = None }
          DeleteForward = KeyBind.create { Key = ConsoleKey.Delete; Modifier = None } }

    type Model =
        { Focused: bool
          Value: StringBuilder
          SizeLimit: int
          Keymap: Keymap
          Prompt: string
          PromptStyle: Style
          TextStyle: Style
          Position: int
          Cursor: Cursor.Model }

    let setCursorPosition (model: Model) pos =
        let p = Math.Clamp(pos, 0, model.Value.Length)
        { model with Position = p }

    let init () =
        let cursor = Cursor.init 1
        { Focused = false
          Value = StringBuilder()
          Keymap = defaultKeymap
          SizeLimit = 0
          Prompt = "> "
          PromptStyle = { Style.Default with FgColor = Color.BrightCyan; Bold = true }
          TextStyle = Style.Default
          Position = 0
          Cursor = cursor },
        Cmd.none

    let update msg model =
        match msg with
        | Focus ->
            let cursor, _ = Cursor.update (Cursor.Focus model.Cursor.Id) model.Cursor
            { model with Focused = true; Cursor = cursor }, Cmd.none

        | Unfocus ->
            { model with Focused = false; Cursor = { model.Cursor with Focused = false } }, Cmd.none

        | CursorMsg cmsg ->
            let m, cmd = Cursor.update cmsg model.Cursor
            { model with Cursor = m }, Cmd.map CursorMsg cmd

        | KeyInput cki when model.Focused ->
            if Keymap.``match`` model.Keymap.CharacterForward cki then
                setCursorPosition model (model.Position + 1), Cmd.none
            elif Keymap.``match`` model.Keymap.CharacterBackward cki then
                setCursorPosition model (model.Position - 1), Cmd.none
            elif Keymap.``match`` model.Keymap.DeleteBackward cki then
                if model.Position > 0 then
                    model.Value.Remove(model.Position - 1, 1) |> ignore
                    setCursorPosition model (model.Position - 1), Cmd.none
                else model, Cmd.none
            elif Keymap.``match`` model.Keymap.DeleteForward cki then
                if model.Position < model.Value.Length then
                    model.Value.Remove(model.Position, 1) |> ignore
                    model, Cmd.none
                else model, Cmd.none
            elif not (Char.IsControl cki.KeyChar) then
                model.Value.Insert(model.Position, cki.KeyChar) |> ignore
                setCursorPosition model (model.Position + 1), Cmd.none
            else
                model, Cmd.none

        | KeyInput _ -> model, Cmd.none

    let subscribe model =
        if model.Focused then
            Sub.map "input" CursorMsg (Cursor.subscribe model.Cursor)
        else
            []

    let view model : Node =
        let s = model.Value.ToString()
        let before = if model.Position > 0 then s.Substring(0, model.Position) else ""
        let after = if model.Position < s.Length then s.Substring(model.Position) else ""
        let cursorNode = Cursor.view model.Cursor

        Node.row [
            Node.styledText model.PromptStyle model.Prompt
            Node.styledText model.TextStyle before
            cursorNode
            Node.styledText model.TextStyle after
        ]
