namespace Fansi

open System
open System.Globalization
open System.Text
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
            /// Cursor index, 0 .. Value.Length. Always on a character boundary: never inside an
            /// emoji or between a letter and its accent.
            Pos: int
            /// First visible character.
            Offset: int
            /// Visible terminal cells of text, 0 = unbounded. The prompt is not counted. The
            /// caller sets it, because the layout only knows the real width after view has run.
            Width: int
            /// Most characters the value may hold (an emoji or a letter with its accent counts
            /// as one), 0 = unlimited.
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

    // A code point with no width of its own (an accent, a zero width joiner, a
    // variation selector) is part of the character before it.
    let private joinsPrevious (r: Rune) =
        Width.ofRune r = 0 && Rune.GetUnicodeCategory r <> UnicodeCategory.Control

    /// Where each character of s starts, then s.Length. Positions only ever sit on
    /// these, so the cursor never lands inside an emoji or between a letter and its
    /// accent, and a delete always removes whole characters.
    let private boundaries (s: string) =
        let starts = ResizeArray<int>()
        let mutable i = 0

        while i < s.Length do
            let mutable r = Rune.ReplacementChar
            // Half a surrogate pair is not a rune. It counts as a character of its own.
            let ok = Rune.TryGetRuneAt(s, i, &r)

            if i = 0 || not ok || not (joinsPrevious r) then
                starts.Add i

            i <- i + (if ok then r.Utf16SequenceLength else 1)

        starts.Add s.Length
        List.ofSeq starts

    let private snap (s: string) pos =
        let pos = Math.Clamp(pos, 0, s.Length)
        boundaries s |> List.find (fun b -> b >= pos)

    let private nextPos (s: string) pos =
        boundaries s |> List.tryFind (fun b -> b > pos) |> Option.defaultValue s.Length

    let private prevPos (s: string) pos =
        boundaries s
        |> List.filter (fun b -> b < pos)
        |> List.tryLast
        |> Option.defaultValue 0

    // Judges each character by its first code unit, so a space that carries an
    // accent still counts as a space and the motion always makes progress.
    let private isSpaceAt (s: string) pos = Char.IsWhiteSpace s[pos]

    let private wordForward (s: string) pos =
        let mutable i = snap s pos

        while i < s.Length && isSpaceAt s i do
            i <- nextPos s i

        while i < s.Length && not (isSpaceAt s i) do
            i <- nextPos s i

        i

    let private wordBackward (s: string) pos =
        let mutable i = snap s pos

        while i > 0 && isSpaceAt s (prevPos s i) do
            i <- prevPos s i

        while i > 0 && not (isSpaceAt s (prevPos s i)) do
            i <- prevPos s i

        i

    let private glyphCount (s: string) = (boundaries s).Length - 1

    /// How many code units of text are joining code points at its very start.
    let private markRun (text: string) =
        let mutable i = 0
        let mutable r = Rune.ReplacementChar

        while i < text.Length && Rune.TryGetRuneAt(text, i, &r) && joinsPrevious r do
            i <- i + r.Utf16SequenceLength

        i

    let private room (model: Model) =
        if model.CharLimit <= 0 then
            Int32.MaxValue
        else
            max 0 (model.CharLimit - glyphCount model.Value)

    /// The first `limit` characters of text, never cut inside one.
    let private fitTo limit (text: string) =
        let starts = boundaries text

        if limit >= starts.Length - 1 then
            text
        else
            text.Substring(0, starts[max 0 limit])

    // Drops what the screen would drop: control and bidi characters, and half a
    // surrogate pair. Accents stay, so one pasted after a letter joins it.
    let private oneLine (text: string) =
        let kept = StringBuilder()
        let mutable i = 0

        while i < text.Length do
            let mutable r = Rune.ReplacementChar

            if Rune.TryGetRuneAt(text, i, &r) then
                if
                    Rune.GetUnicodeCategory r <> UnicodeCategory.Control
                    && not (Width.isBidiControl r)
                then
                    kept.Append(r.ToString()) |> ignore

                i <- i + r.Utf16SequenceLength
            else
                i <- i + 1

        kept.ToString()

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
            |> List.filter (fun s -> model.CharLimit <= 0 || glyphCount s <= model.CharLimit)
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

    let private echo (model: Model) (s: string) =
        match model.Echo with
        | Normal -> s
        | Password mask -> String(mask, glyphCount s)
        | Hidden -> ""

    /// Cells one character takes on screen, as the echo mode shows it.
    let private shownWidth (model: Model) (glyph: string) =
        match model.Echo with
        | Normal -> Width.ofString glyph
        | Password mask -> Width.ofString (string mask)
        | Hidden -> 0

    /// Each character of the value as (start, end, cells shown).
    let private spans (model: Model) =
        boundaries model.Value
        |> List.pairwise
        |> List.map (fun (a, b) -> a, b, shownWidth model (model.Value.Substring(a, b - a)))
        |> Array.ofList

    /// Cells shown between two boundaries of the value. Prefix sums keep it fast on
    /// a long value, where scrolling asks this once per character.
    let private cellsBetween (spans: (int * int * int) array) =
        let starts = spans |> Array.map (fun (a, _, _) -> a)
        let prefix = spans |> Array.map (fun (_, _, c) -> c) |> Array.scan (+) 0

        let at pos =
            match Array.BinarySearch(starts, pos) with
            | i when i >= 0 -> i
            | _ -> spans.Length

        fun a b -> prefix[at b] - prefix[at a]

    /// The longest start of text that fits in `cells` cells, cut between characters.
    let private fitCells cells (text: string) =
        let mutable used = 0
        let mutable stop = 0
        let mutable full = false

        for (a, b) in List.pairwise (boundaries text) do
            if not full then
                let c = Width.ofString (text.Substring(a, b - a))

                if used + c > cells then
                    full <- true
                else
                    used <- used + c
                    stop <- b

        text.Substring(0, stop)

    let private scroll (model: Model) =
        if model.Width <= 0 then
            { model with Offset = 0 }
        else
            let spans = spans model
            let between = cellsBetween spans

            let starts =
                Array.append (spans |> Array.map (fun (a, _, _) -> a)) [| model.Value.Length |]

            let start = snap model.Value model.Offset

            // The cursor cell is the character under the cursor, or one blank cell at
            // the end. A hidden character still needs a cell for the cursor.
            let cursorCells =
                spans
                |> Array.tryFind (fun (a, _, _) -> a = model.Pos)
                |> Option.map (fun (_, _, c) -> max 1 c)
                |> Option.defaultValue 1

            let fits a =
                between a model.Pos + cursorCells <= model.Width

            let offset =
                if model.Pos < start then
                    model.Pos
                elif fits start then
                    start
                else
                    starts
                    |> Array.tryFind (fun a -> a > start && a <= model.Pos && fits a)
                    |> Option.defaultValue model.Pos

            // Pulled back as far as the value allows, so deleting at the end shows
            // more text instead of leaving a gap on the right.
            let earliest =
                starts
                |> Array.find (fun a -> a = model.Value.Length || between a model.Value.Length + 1 <= model.Width)

            { model with
                Offset = min offset earliest }

    // The caller may write Width, Pos or Value directly, not just through update
    // (setting layout width, clearing a field, seeding a value), so both update
    // and view run this first to make the model consistent with itself. It
    // never touches History: a hand-edit is not something to undo.
    let private normalise (model: Model) =
        let pos = snap model.Value model.Pos

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
        let insert () =
            commitTyped (model.Value.Insert(model.Pos, string c)) (model.Pos + 1) model

        if Char.IsHighSurrogate c then
            if room model >= 1 then insert () else model
        elif Char.IsLowSurrogate c then
            // The second half of an emoji joins the first, which already took the slot.
            if model.Pos > 0 && Char.IsHighSurrogate(model.Value[model.Pos - 1]) then
                insert ()
            else
                model
        else
            let r = Rune c

            if Width.isBidiControl r then
                model
            elif joinsPrevious r then
                // An accent joins the letter before the cursor and takes no slot.
                if model.Pos > 0 then insert () else model
            elif room model >= 1 then
                insert ()
            else
                model

    let private paste (text: string) (model: Model) =
        let text = oneLine text
        let marks = markRun text

        // Accents at the start of a paste join the letter before the cursor, free of
        // charge. With no letter there they have nothing to sit on.
        let lead = if model.Pos > 0 then text.Substring(0, marks) else ""
        let text = lead + (text.Substring(marks) |> fitTo (room model))

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

        let text = oneLine text
        let text = text.Substring(markRun text) |> fitTo limit
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

    let view (focused: bool) (model: Model) : Node =
        let model = normalise model
        let prompt = Ui.text model.Prompt |> Ui.style model.PromptStyle
        let width = if model.Width <= 0 then Int32.MaxValue else model.Width

        let cursor under style =
            Cursor.view focused under style model.Cursor

        if model.Value = "" then
            let ph = fitCells width model.Placeholder
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

            let spans = spans model
            let between = cellsBetween spans
            let underEnd = nextPos v model.Pos
            let before = v.Substring(model.Offset, model.Pos - model.Offset)
            let under = v.Substring(model.Pos, underEnd - model.Pos)
            // Scrolling leaves at least one cell here, so the cursor always shows.
            let left = width - between model.Offset model.Pos

            let ghost =
                match currentSuggestion model with
                | Some s when focused -> s.Substring(v.Length)
                | _ -> ""

            // Gluing the accent onto the typed text would draw a different letter than
            // the user typed, so a ghost that starts with one is not shown.
            let ghost = if markRun ghost > 0 then "" else ghost

            let ghostHead = ghost.Substring(0, nextPos ghost 0)

            let underShown, underStyle =
                let shown, style =
                    if under = "" && ghostHead <> "" then
                        ghostHead, model.SuggestionStyle
                    else
                        echo model under, textStyle

                // A character wider than the cells left, as in a field one cell wide,
                // cannot show. The cursor still needs its cell.
                if Width.ofString shown = 0 || Width.ofString shown > left then
                    " ", style
                else
                    shown, style

            let left = left - Width.ofString underShown

            // Whole characters after the cursor while they fit. A wide one that would
            // cross the edge is left out.
            let stop, _, _ =
                spans
                |> Array.filter (fun (a, _, _) -> a >= underEnd)
                |> Array.fold
                    (fun (stop, room, full) (_, b, c) ->
                        if full || c > room then
                            stop, room, true
                        else
                            b, room - c, false)
                    (underEnd, left, false)

            let after = v.Substring(underEnd, stop - underEnd)

            let ghostTail =
                if underShown = ghostHead then
                    ghost.Substring(ghostHead.Length) |> fitCells (left - between underEnd stop)
                else
                    ""

            Ui.row
                [ prompt
                  Ui.text (echo model before) |> Ui.style textStyle
                  cursor underShown underStyle
                  Ui.text (echo model after) |> Ui.style textStyle
                  Ui.text ghostTail |> Ui.style model.SuggestionStyle ]

    let subscribe (focused: bool) (model: Model) =
        Cursor.subscribe focused model.Cursor |> Sub.map CursorMsg
