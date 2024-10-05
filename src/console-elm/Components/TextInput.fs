namespace Fansi

open System
open Elmish
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
        | Blink of CursorId

    type Model = {
        Id: CursorId
        Type: CursorType
        BlinkSpeed: float
        Focused: bool
        Char: char
        Blink: bool
    }
    
    let init id =
        { Id = id; Type = CursorType.Blink; Char = '|'; Focused = false; Blink = true; BlinkSpeed = 530 }
        
    let update msg model =
        match msg with
        | Focus id ->
            if model.Id <> id then
                model, Cmd.none
            else
                { model with Focused = true; Blink = model.Type = CursorType.Hidden }, Cmd.none
        | Blink id ->
            if model.Id <> id || not model.Focused || model.Type <> CursorType.Blink then
                model, Cmd.none
            else if model.Type = CursorType.Blink then
                { model with Blink = not model.Blink }, Cmd.none
            else
                model, Cmd.none
    let view model =
        if model.Blink then 
            string model.Char
        else ""
        
    let subscribe model =
        [ if model.Focused then
            ["cursor"; string model.Id], Sub.timer model.BlinkSpeed (Message.Blink model.Id) ]

[<RequireQualifiedAccess>]
module TextInputComponent =
        
    type Message =
        | Focus
        | CursorMsg of Cursor.Message
        
    type Keymap = {
        CharacterForward: KeyBind
        CharacterBackward: KeyBind
        Paste: KeyBind
    }
    
    let defaultKeymap = {
        CharacterForward = KeyBind.create { Key = ConsoleKey.RightArrow; Modifier = None }
        CharacterBackward =  KeyBind.create { Key = ConsoleKey.LeftArrow; Modifier = None }
        Paste = KeyBind.create { Key = ConsoleKey.V; Modifier = Some ConsoleModifiers.Control } 
    }
    
    type Model = {
        Focused: bool
        Value: string
        SizeLimit: int
        Keymap: Keymap
        Prompt: string
        Position: int
        Cursor: Cursor.Model
    }

    let init () =
        let cursor = Cursor.init 1
        { Focused = false; Value = ""; Keymap = defaultKeymap; SizeLimit = 0; Prompt = "> "; Position = 0; Cursor = cursor }, Cmd.none
    
    let update msg model =
        match msg with 
        | Focus ->
            let cursor, _ = Cursor.update (Cursor.Focus model.Cursor.Id) model.Cursor
            { model with Focused = true; Cursor = cursor }, Cmd.none
        
        | CursorMsg msg ->
            let m, cmd = Cursor.update msg model.Cursor
            { model with Cursor = m }, Cmd.map CursorMsg cmd
        
    let subscribe model =
        if model.Focused then
            Sub.map "input" CursorMsg (Cursor.subscribe model.Cursor)
        else
            []
        
    let view model =
        let c = Cursor.view model.Cursor
        $"{model.Prompt}{c}"