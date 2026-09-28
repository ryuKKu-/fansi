namespace Fansi

open System
open Elmish
open Fansi
open Fansi.Core
open Fansi.Keymap

[<RequireQualifiedAccess>]
module TextInputComponent =

    type Echo =
        | Normal
        | Password of char
        | Hidden

    type Validation =
        | Valid
        | Invalid of string

    type History =
        {
            Past: (string * int) list
            Future: (string * int) list
            /// True while typed characters are going into the same undo step.
            Typing: bool
        }

    type Message =
        | KeyInput of KeyEvent
        | Pasted of string
        | SetValue of string
        | CursorMsg of Cursor.Message

    type Keymap =
        { CharacterForward: KeyBind
          CharacterBackward: KeyBind
          WordForward: KeyBind
          WordBackward: KeyBind
          LineStart: KeyBind
          LineEnd: KeyBind
          DeleteBackward: KeyBind
          DeleteForward: KeyBind
          DeleteWordBackward: KeyBind
          DeleteWordForward: KeyBind
          DeleteToLineEnd: KeyBind
          DeleteToLineStart: KeyBind
          Undo: KeyBind
          Redo: KeyBind
          AcceptSuggestion: KeyBind
          NextSuggestion: KeyBind
          PrevSuggestion: KeyBind }

    let private ctrl c = KeyEvent.ctrl (Key.Char c)
    let private alt c = KeyEvent.alt (Key.Char c)

    let defaultKeymap =
        { CharacterForward = KeyBind.create [ KeyEvent.plain Key.Right; ctrl 'f' ]
          CharacterBackward = KeyBind.create [ KeyEvent.plain Key.Left; ctrl 'b' ]
          WordForward = KeyBind.create [ KeyEvent.ctrl Key.Right; alt 'f' ]
          WordBackward = KeyBind.create [ KeyEvent.ctrl Key.Left; alt 'b' ]
          LineStart = KeyBind.create [ KeyEvent.plain Key.Home; ctrl 'a' ]
          LineEnd = KeyBind.create [ KeyEvent.plain Key.End; ctrl 'e' ]
          DeleteBackward = KeyBind.create [ KeyEvent.plain Key.Backspace ]
          DeleteForward = KeyBind.create [ KeyEvent.plain Key.Delete; ctrl 'd' ]
          // Many terminals send the same byte for Backspace and Ctrl+Backspace, so
          // the readline keys stay bound as a way that always works.
          DeleteWordBackward = KeyBind.create [ KeyEvent.ctrl Key.Backspace; ctrl 'w'; KeyEvent.alt Key.Backspace ]
          DeleteWordForward = KeyBind.create [ alt 'd' ]
          DeleteToLineEnd = KeyBind.create [ ctrl 'k' ]
          DeleteToLineStart = KeyBind.create [ ctrl 'u' ]
          Undo = KeyBind.create [ ctrl 'z' ]
          Redo = KeyBind.create [ ctrl 'y' ]
          AcceptSuggestion = KeyBind.create [ KeyEvent.plain Key.Tab ]
          NextSuggestion = KeyBind.create [ ctrl 'n' ]
          PrevSuggestion = KeyBind.create [ ctrl 'p' ] }

    type Model =
        {
            Value: string
            /// Cursor index, 0 .. Value.Length. Never between the halves of a surrogate pair.
            Pos: int
            /// First visible character.
            Offset: int
            /// Visible cells, 0 = unbounded. The caller sets it, because the layout
            /// only knows the real width after view has run.
            Width: int
            /// 0 = unlimited.
            CharLimit: int
            Prompt: string
            Placeholder: string
            Echo: Echo
            Validate: string -> Validation
            Error: string option
            Suggestions: string list
            SuggestionIndex: int
            History: History
            Keymap: Keymap
            Cursor: Cursor.Model
            PromptStyle: Style
            TextStyle: Style
            PlaceholderStyle: Style
            ErrorStyle: Style
            SuggestionStyle: Style
        }

    let init () =
        { Value = ""
          Pos = 0
          Offset = 0
          Width = 0
          CharLimit = 0
          Prompt = "> "
          Placeholder = ""
          Echo = Normal
          Validate = (fun _ -> Valid)
          Error = None
          Suggestions = []
          SuggestionIndex = 0
          History =
            { Past = []
              Future = []
              Typing = false }
          Keymap = defaultKeymap
          Cursor = Cursor.create ()
          PromptStyle =
            { Style.Default with
                FgColor = Color.BrightCyan
                Bold = true }
          TextStyle = Style.Default
          PlaceholderStyle =
            { Style.Default with
                FgColor = Color.BrightBlack }
          ErrorStyle =
            { Style.Default with
                FgColor = Color.Red }
          SuggestionStyle =
            { Style.Default with
                FgColor = Color.BrightBlack } },
        Cmd.none

    // Positions step over a surrogate pair as one character, so the cursor never
    // splits an emoji and a delete never leaves half of one behind.
    let private nextPos (s: string) pos =
        if pos >= s.Length then
            s.Length
        elif pos + 1 < s.Length && Char.IsSurrogatePair(s[pos], s[pos + 1]) then
            pos + 2
        else
            pos + 1

    let private prevPos (s: string) pos =
        if pos <= 0 then
            0
        elif pos >= 2 && Char.IsSurrogatePair(s[pos - 2], s[pos - 1]) then
            pos - 2
        else
            pos - 1

    let private wordForward (s: string) pos =
        let mutable i = pos

        while i < s.Length && Char.IsWhiteSpace(s[i]) do
            i <- i + 1

        while i < s.Length && not (Char.IsWhiteSpace(s[i])) do
            i <- i + 1

        i

    let private wordBackward (s: string) pos =
        let mutable i = pos

        while i > 0 && Char.IsWhiteSpace(s[i - 1]) do
            i <- i - 1

        while i > 0 && not (Char.IsWhiteSpace(s[i - 1])) do
            i <- i - 1

        i

    let private room (model: Model) =
        if model.CharLimit <= 0 then
            Int32.MaxValue
        else
            max 0 (model.CharLimit - model.Value.Length)

    /// Cuts text to `limit` characters without keeping half a surrogate pair.
    let private fitTo limit (text: string) =
        if text.Length <= limit then
            text
        elif limit > 0 && Char.IsHighSurrogate(text[limit - 1]) then
            text.Substring(0, limit - 1)
        else
            text.Substring(0, limit)

    let private oneLine (text: string) =
        let clean = text |> Seq.filter (fun c -> not (Char.IsControl c)) |> Array.ofSeq
        let kept = ResizeArray<char>(clean.Length)
        let mutable i = 0

        while i < clean.Length do
            let c = clean[i]

            if
                Char.IsHighSurrogate c
                && i + 1 < clean.Length
                && Char.IsLowSurrogate clean[i + 1]
            then
                kept.Add c
                kept.Add clean[i + 1]
                i <- i + 2
            elif Char.IsSurrogate c then
                // Half a pair with no partner next to it cannot be rendered or
                // measured, so it is dropped rather than let into the value.
                i <- i + 1
            else
                kept.Add c
                i <- i + 1

        String(kept.ToArray())

    let private validate (model: Model) =
        { model with
            Error =
                match model.Validate model.Value with
                | Valid -> None
                | Invalid reason -> Some reason }

    let withValidation (check: string -> Validation) (model: Model) =
        validate { model with Validate = check }

    let private matches (model: Model) =
        // A suggestion on a hidden value would print the rest of it in clear.
        if model.Echo <> Normal || model.Value = "" || model.Pos <> model.Value.Length then
            []
        else
            model.Suggestions
            // Cleaned to one line and cut to the limit up front, so whatever comes out
            // is already safe to commit as-is.
            |> List.map oneLine
            |> List.filter (fun s -> model.CharLimit <= 0 || s.Length <= model.CharLimit)
            |> List.filter (fun s ->
                s.Length > model.Value.Length
                && s.StartsWith(model.Value, StringComparison.OrdinalIgnoreCase))

    let currentSuggestion (model: Model) =
        match matches model with
        | [] -> None
        // The caller can change the pool, or set a negative index, at any time.
        | found -> Some found[((model.SuggestionIndex % found.Length) + found.Length) % found.Length]

    let private cycle step (model: Model) =
        match (matches model).Length with
        | 0 -> model
        | count ->
            { model with
                SuggestionIndex = ((model.SuggestionIndex + step) % count + count) % count }

    let private scroll (model: Model) =
        if model.Width <= 0 then
            { model with Offset = 0 }
        else
            let w = model.Width

            let offset =
                if model.Pos < model.Offset then model.Pos
                elif model.Pos >= model.Offset + w then model.Pos - w + 1
                else model.Offset

            // Value.Length + 1: at the end of the value the cursor sits on a cell of
            // its own, and that cell has to stay on screen too.
            let offset = Math.Clamp(offset, 0, max 0 (model.Value.Length + 1 - w))

            // Never start the view on the second half of a pair. Pos is never there,
            // so moving right cannot pass it.
            let offset =
                if
                    offset > 0
                    && offset < model.Value.Length
                    && Char.IsLowSurrogate(model.Value[offset])
                then
                    offset + 1
                else
                    offset

            { model with Offset = offset }

    // The caller may write Width, Pos or Value directly, not just through update
    // (setting layout width, clearing a field, seeding a value), so both update
    // and view run this first to make the model consistent with itself. It
    // never touches History: a hand-edit is not something to undo.
    let private normalise (model: Model) =
        let pos = Math.Clamp(model.Pos, 0, model.Value.Length)

        let pos =
            if pos < model.Value.Length && Char.IsLowSurrogate(model.Value[pos]) then
                pos + 1
            else
                pos

        { model with Pos = pos } |> validate |> scroll

    let private historyDepth = 100

    let private moveTo pos (model: Model) =
        { model with
            Pos = pos
            History = { model.History with Typing = false } }
        |> scroll

    let private apply typed value pos (model: Model) =
        if value = model.Value then
            moveTo pos model
        else
            let h = model.History

            let past =
                if typed && h.Typing then
                    h.Past
                else
                    (model.Value, model.Pos) :: h.Past |> List.truncate historyDepth

            { model with
                Value = value
                Pos = pos
                SuggestionIndex = 0
                History =
                    { Past = past
                      Future = []
                      Typing = typed } }
            |> validate
            |> scroll

    let private commit value pos model = apply false value pos model

    let private commitTyped value pos model = apply true value pos model

    let private restore (value, pos) history (model: Model) =
        { model with
            Value = value
            Pos = pos
            SuggestionIndex = 0
            History = history }
        |> validate
        |> scroll

    let private undo (model: Model) =
        match model.History.Past with
        | state :: rest ->
            restore
                state
                { Past = rest
                  Future = (model.Value, model.Pos) :: model.History.Future
                  Typing = false }
                model
        | [] -> model

    let private redo (model: Model) =
        match model.History.Future with
        | state :: rest ->
            restore
                state
                { Past = (model.Value, model.Pos) :: model.History.Past |> List.truncate historyDepth
                  Future = rest
                  Typing = false }
                model
        | [] -> model

    let private insertChar (c: char) (model: Model) =
        let free = room model

        // An emoji comes as two keys. Its first half only goes in if the second
        // half will fit too, and a second half with no first half is dropped.
        let fits =
            if Char.IsHighSurrogate c then
                free >= 2
            elif Char.IsLowSurrogate c then
                free >= 1 && model.Pos > 0 && Char.IsHighSurrogate(model.Value[model.Pos - 1])
            else
                free >= 1

        if fits then
            commitTyped (model.Value.Insert(model.Pos, string c)) (model.Pos + 1) model
        else
            model

    let private paste (text: string) (model: Model) =
        let text = oneLine text |> fitTo (room model)

        if text = "" then
            moveTo model.Pos model
        else
            commit (model.Value.Insert(model.Pos, text)) (model.Pos + text.Length) model

    let private setValue (text: string) (model: Model) =
        let limit =
            if model.CharLimit <= 0 then
                Int32.MaxValue
            else
                model.CharLimit

        let text = oneLine text |> fitTo limit
        commit text text.Length model

    type private Action =
        | CharacterForward
        | CharacterBackward
        | WordForward
        | WordBackward
        | LineStart
        | LineEnd
        | DeleteBackward
        | DeleteForward
        | DeleteWordBackward
        | DeleteWordForward
        | DeleteToLineEnd
        | DeleteToLineStart
        | Undo
        | Redo
        | AcceptSuggestion
        | NextSuggestion
        | PrevSuggestion

    let private actionFor (keymap: Keymap) (key: KeyEvent) =
        [ keymap.CharacterForward, CharacterForward
          keymap.CharacterBackward, CharacterBackward
          keymap.WordForward, WordForward
          keymap.WordBackward, WordBackward
          keymap.LineStart, LineStart
          keymap.LineEnd, LineEnd
          keymap.DeleteBackward, DeleteBackward
          keymap.DeleteForward, DeleteForward
          keymap.DeleteWordBackward, DeleteWordBackward
          keymap.DeleteWordForward, DeleteWordForward
          keymap.DeleteToLineEnd, DeleteToLineEnd
          keymap.DeleteToLineStart, DeleteToLineStart
          keymap.Undo, Undo
          keymap.Redo, Redo
          keymap.AcceptSuggestion, AcceptSuggestion
          keymap.NextSuggestion, NextSuggestion
          keymap.PrevSuggestion, PrevSuggestion ]
        |> List.tryPick (fun (bind, action) -> if Keymap.``match`` bind key then Some action else None)

    let private perform action (model: Model) =
        let v, p = model.Value, model.Pos

        let deleteRange start stop =
            commit (v.Remove(start, stop - start)) start model

        match action with
        | CharacterForward -> moveTo (nextPos v p) model
        | CharacterBackward -> moveTo (prevPos v p) model
        | WordForward -> moveTo (wordForward v p) model
        | WordBackward -> moveTo (wordBackward v p) model
        | LineStart -> moveTo 0 model
        | LineEnd -> moveTo v.Length model
        | DeleteBackward -> deleteRange (prevPos v p) p
        | DeleteForward -> deleteRange p (nextPos v p)
        | DeleteWordBackward -> deleteRange (wordBackward v p) p
        | DeleteWordForward -> deleteRange p (wordForward v p)
        | DeleteToLineEnd -> deleteRange p v.Length
        | DeleteToLineStart -> deleteRange 0 p
        | Undo -> undo model
        | Redo -> redo model
        | AcceptSuggestion ->
            match currentSuggestion model with
            | Some s -> commit s s.Length model
            | None -> model
        | NextSuggestion -> cycle 1 model
        | PrevSuggestion -> cycle -1 model

    let update msg (model: Model) =
        match msg with
        | KeyInput key ->
            let model = normalise model

            match actionFor model.Keymap key with
            | Some action -> perform action model, Cmd.none
            | None ->
                match key.Key with
                | Key.Char c when not key.Ctrl && not key.Alt && not (Char.IsControl c) -> insertChar c model, Cmd.none
                | _ -> model, Cmd.none
        | Pasted text -> paste text (normalise model), Cmd.none
        | SetValue text -> setValue text (normalise model), Cmd.none
        | CursorMsg m ->
            let cursor, cmd = Cursor.update m model.Cursor
            { model with Cursor = cursor }, Cmd.map CursorMsg cmd

    let private echo (model: Model) (s: string) =
        match model.Echo with
        | Normal -> s
        | Password mask -> String(mask, s |> Seq.filter (fun c -> not (Char.IsLowSurrogate c)) |> Seq.length)
        | Hidden -> ""

    let view (focused: bool) (model: Model) : Node =
        let model = normalise model
        let prompt = Ui.text model.Prompt |> Ui.style model.PromptStyle

        let cursor under style =
            Cursor.view focused under style model.Cursor

        if model.Value = "" then
            let ph = model.Placeholder
            let split = nextPos ph 0
            let under = if ph = "" then " " else ph.Substring(0, split)

            Ui.row
                [ prompt
                  cursor under model.PlaceholderStyle
                  Ui.text (ph.Substring(split)) |> Ui.style model.PlaceholderStyle ]
        else
            let v = model.Value

            let textStyle =
                if model.Error.IsSome then
                    model.ErrorStyle
                else
                    model.TextStyle

            let stop =
                let raw =
                    if model.Width <= 0 then
                        v.Length
                    else
                        min v.Length (model.Offset + model.Width)

                // The right edge counts UTF-16 units, so it can land between the
                // halves of a pair. Pull it back rather than show half an emoji.
                if raw < v.Length && Char.IsLowSurrogate(v[raw]) then
                    raw - 1
                else
                    raw

            let underEnd = nextPos v model.Pos
            let before = v.Substring(model.Offset, model.Pos - model.Offset)
            let under = v.Substring(model.Pos, underEnd - model.Pos)

            let after =
                if underEnd < stop then
                    v.Substring(underEnd, stop - underEnd)
                else
                    ""

            let ghost =
                match currentSuggestion model with
                | Some s when focused -> s.Substring(v.Length)
                | _ -> ""

            let ghostHead = ghost.Substring(0, nextPos ghost 0)

            // Never negative: scroll clamps Offset so that Pos - Offset <= Width - 1,
            // leaving at least one cell for the cursor itself.
            let ghostRoom =
                if model.Width <= 0 then
                    Int32.MaxValue
                else
                    max 0 (model.Width - (model.Pos - model.Offset) - 1)

            let ghostTail = ghost.Substring(ghostHead.Length) |> fitTo ghostRoom

            let underShown, underStyle =
                if under = "" && ghostHead <> "" then
                    ghostHead, model.SuggestionStyle
                else
                    match echo model under with
                    | "" -> " ", textStyle
                    | s -> s, textStyle

            Ui.row
                [ prompt
                  Ui.text (echo model before) |> Ui.style textStyle
                  cursor underShown underStyle
                  Ui.text (echo model after) |> Ui.style textStyle
                  Ui.text ghostTail |> Ui.style model.SuggestionStyle ]

    let subscribe (focused: bool) (model: Model) =
        Cursor.subscribe focused model.Cursor |> Sub.map CursorMsg
