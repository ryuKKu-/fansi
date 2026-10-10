namespace Fansi.Core

open System
open System.Text

module InputParser =

    /// What one step of the parser did with the front of the buffer.
    type private Step =
        | Emitted of InputEvent list * int
        /// Bytes understood but producing nothing the app needs to see.
        | Skipped of int
        /// The buffer ends mid-sequence. Wait for more bytes.
        | Incomplete

    /// The byte length of the character that this UTF-8 lead byte starts. 0 if
    /// it is not a lead byte.
    let private charLength (b: byte) =
        if b < 0x80uy then 1
        elif b >= 0xf0uy then 4
        elif b >= 0xe0uy then 3
        elif b >= 0xc0uy then 2
        else 0

    let private key k = InputEvent.Key(KeyEvent.plain k)

    let private ctrlKey k =
        InputEvent.Key
            { Key = k
              Ctrl = true
              Alt = false
              Shift = false }

    let private control (b: byte) =
        match b with
        | 0x0duy
        | 0x0auy -> Emitted([ key Key.Enter ], 1)
        | 0x09uy -> Emitted([ key Key.Tab ], 1)
        | 0x7fuy -> Emitted([ key Key.Backspace ], 1)
        // Terminals disagree about which byte Ctrl+Backspace sends. Keep both
        // bytes separate.
        | 0x08uy -> Emitted([ ctrlKey Key.Backspace ], 1)
        | b when b >= 0x01uy && b <= 0x1auy -> Emitted([ ctrlKey (Key.Char(char (b + 96uy))) ], 1)
        | _ -> Skipped 1

    let private text (buffer: ReadOnlySpan<byte>) =
        let width = charLength buffer[0]

        if width = 0 then
            // A continuation byte with no lead byte. Drop it. Do not stall.
            Skipped 1
        elif width > buffer.Length then
            Incomplete
        else
            let decoded = Encoding.UTF8.GetString(buffer.Slice(0, width))
            // An astral character decodes to a UTF-16 surrogate pair. Emit one
            // Key.Char per code unit. Do not drop the second half. A consumer
            // joins the pair again by appending.
            let events = decoded |> Seq.map (Key.Char >> key) |> List.ofSeq
            Emitted(events, width)

    let private withModifiers (parameter: int) k =
        // The parameter is 1 plus a bitmask: 1 Shift, 2 Alt, 4 Ctrl.
        let bits = parameter - 1

        InputEvent.Key
            { Key = k
              Shift = bits &&& 1 <> 0
              Alt = bits &&& 2 <> 0
              Ctrl = bits &&& 4 <> 0 }

    let private tildeKey n =
        match n with
        | 1
        | 7 -> Some Key.Home
        | 2 -> Some Key.Insert
        | 3 -> Some Key.Delete
        | 4
        | 8 -> Some Key.End
        | 5 -> Some Key.PageUp
        | 6 -> Some Key.PageDown
        | 15 -> Some(Key.F 5)
        | 17 -> Some(Key.F 6)
        | 18 -> Some(Key.F 7)
        | 19 -> Some(Key.F 8)
        | 20 -> Some(Key.F 9)
        | 21 -> Some(Key.F 10)
        | 23 -> Some(Key.F 11)
        | 24 -> Some(Key.F 12)
        | _ -> None

    let private letterKey (c: char) =
        match c with
        | 'A' -> Some Key.Up
        | 'B' -> Some Key.Down
        | 'C' -> Some Key.Right
        | 'D' -> Some Key.Left
        | 'H' -> Some Key.Home
        | 'F' -> Some Key.End
        | _ -> None

    /// Internal so the reader can tell a stalled paste from a malformed tail.
    /// The reader also uses it to find the end of a paste that it started to
    /// pass on.
    [<Literal>]
    let internal PasteStart = "\x1b[200~"

    [<Literal>]
    let internal PasteEnd = "\x1b[201~"

    let private PasteStartBytes = Encoding.ASCII.GetBytes PasteStart
    let private PasteEndBytes = Encoding.ASCII.GetBytes PasteEnd

    /// SGR mouse: ESC [ < button ; x ; y then M for press or m for release.
    /// Coordinates arrive one-based. The parser reports them zero-based, to
    /// match Rect.
    let private mouse (buffer: ReadOnlySpan<byte>) =
        let mutable i = 3
        let mutable finalAt = -1

        while finalAt < 0 && i < buffer.Length do
            let c = char buffer[i]
            if c = 'M' || c = 'm' then finalAt <- i else i <- i + 1

        if finalAt < 0 then
            Incomplete
        else
            let final = char buffer[finalAt]
            let body = Encoding.ASCII.GetString(buffer.Slice(3, finalAt - 3))
            let parts = body.Split(';')
            let consumed = finalAt + 1

            let parsed =
                parts
                |> Array.map (fun p ->
                    match Int32.TryParse p with
                    | true, v -> Some v
                    | _ -> None)

            match parsed with
            | [| Some code; Some x; Some y |] ->
                let action =
                    if final = 'm' then MouseAction.Release
                    elif code &&& 32 <> 0 then MouseAction.Move
                    else MouseAction.Press

                let button =
                    if code &&& 64 <> 0 then
                        if code &&& 3 = 0 then
                            MouseButton.ScrollUp
                        else
                            MouseButton.ScrollDown
                    else
                        match code &&& 3 with
                        | 0 -> MouseButton.Left
                        | 1 -> MouseButton.Middle
                        | 2 -> MouseButton.Right
                        | _ -> MouseButton.None

                Emitted(
                    [ InputEvent.Mouse
                          { Button = button
                            Action = action
                            X = x - 1
                            Y = y - 1
                            Ctrl = code &&& 16 <> 0
                            Alt = code &&& 8 <> 0
                            Shift = code &&& 4 <> 0 } ],
                    consumed
                )
            | _ -> Skipped consumed

    /// Everything between the paste markers is text. This includes bytes that
    /// look like escape sequences. Without the terminator, the whole paste
    /// waits. A paste bigger than one read therefore still arrives whole. Find
    /// the terminator in the raw bytes. Do not decode the whole buffer first.
    /// Otherwise a paste split across many reads is decoded in full on every
    /// call. Also, a body that is not valid UTF-8 gives a wrong byte count
    /// after a round trip through GetString and GetByteCount.
    let private paste (buffer: ReadOnlySpan<byte>) =
        let bodyStart = PasteStartBytes.Length
        let endAt = buffer.Slice(bodyStart).IndexOf(ReadOnlySpan PasteEndBytes)

        if endAt < 0 then
            Incomplete
        else
            let body = Encoding.UTF8.GetString(buffer.Slice(bodyStart, endAt))
            let consumed = bodyStart + endAt + PasteEndBytes.Length
            Emitted([ InputEvent.Paste body ], consumed)

    /// A CSI sequence is ESC [ then digits and semicolons, then one final byte
    /// in the range 0x40 to 0x7e. The parser waits for that final byte. This is
    /// how it tells an unfinished sequence from a finished one.
    let private csi (buffer: ReadOnlySpan<byte>) =
        if buffer.Length >= 3 && char buffer[2] = '<' then
            mouse buffer
        elif buffer.Length >= 3 && char buffer[2] = 'I' then
            Emitted([ InputEvent.FocusChanged true ], 3)
        elif buffer.Length >= 3 && char buffer[2] = 'O' then
            Emitted([ InputEvent.FocusChanged false ], 3)
        elif
            buffer.Length >= PasteStart.Length
            && Encoding.ASCII.GetString(buffer.Slice(0, PasteStart.Length)) = PasteStart
        then
            paste buffer
        else

            let mutable i = 2
            let mutable finalAt = -1
            let mutable escAt = -1

            while finalAt < 0 && escAt < 0 && i < buffer.Length do
                let b = buffer[i]

                if b >= 0x40uy && b <= 0x7euy then
                    finalAt <- i
                elif b = 0x1buy then
                    // The next sequence starts before this one has a final
                    // byte. The sequence is malformed. Stop here. Do not scan
                    // through the ESC that begins the next one.
                    escAt <- i
                else
                    i <- i + 1

            if finalAt < 0 then
                if escAt < 0 then Incomplete else Skipped escAt
            else
                let final = char buffer[finalAt]
                let parameterText = Encoding.ASCII.GetString(buffer.Slice(2, finalAt - 2))

                let parameters =
                    parameterText.Split(';')
                    |> Array.map (fun p ->
                        match Int32.TryParse p with
                        | true, v -> v
                        | _ -> 0)

                let modifier = if parameters.Length > 1 then parameters[1] else 1
                let first = if parameters.Length > 0 then parameters[0] else 0
                let consumed = finalAt + 1

                if final = 'Z' then
                    // Shift+Tab. The letter itself gives the shift. There is no
                    // modifier parameter to read.
                    Emitted(
                        [ InputEvent.Key
                              { Key = Key.Tab
                                Ctrl = false
                                Alt = false
                                Shift = true } ],
                        consumed
                    )
                else
                    match letterKey final, tildeKey first with
                    | Some k, _ -> Emitted([ withModifiers modifier k ], consumed)
                    | _, Some k when final = '~' -> Emitted([ withModifiers modifier k ], consumed)
                    | _ -> Skipped consumed

    /// SS3: ESC O then one byte. Only the first four function keys use it.
    let private ss3 (buffer: ReadOnlySpan<byte>) =
        if buffer.Length < 3 then
            Incomplete
        else
            match char buffer[2] with
            | 'P' -> Emitted([ key (Key.F 1) ], 3)
            | 'Q' -> Emitted([ key (Key.F 2) ], 3)
            | 'R' -> Emitted([ key (Key.F 3) ], 3)
            | 'S' -> Emitted([ key (Key.F 4) ], 3)
            | c ->
                // Application cursor mode also sends arrows and Home/End
                // through SS3, not only the four function keys. Use the same
                // table as CSI for these.
                match letterKey c with
                | Some k -> Emitted([ key k ], 3)
                | None -> Skipped 3

    /// Applies Alt to every key in a step's result. It also counts the ESC byte
    /// that the step did not see.
    let private withAlt result =
        match result with
        | Emitted(es, n) ->
            let alt =
                es
                |> List.map (function
                    | InputEvent.Key k -> InputEvent.Key { k with Alt = true }
                    | e -> e)

            Emitted(alt, n + 1)
        | Skipped n -> Skipped(n + 1)
        | Incomplete -> Incomplete

    let private escape (buffer: ReadOnlySpan<byte>) =
        if buffer.Length = 1 then
            // It could be the Escape key. It could be the first byte of a
            // sequence. Only time tells them apart. That is the job of
            // parseFinal.
            Incomplete
        else
            match char buffer[1] with
            | '[' -> csi buffer
            | 'O' -> ss3 buffer
            | _ ->
                let b = buffer[1]

                if b = 0x1buy && buffer.Length = 2 then
                    // A second ESC with nothing after it yet. It might start a
                    // sequence when its "[" or "O" arrives. Wait for it in the
                    // same way as for a lone ESC.
                    Incomplete
                elif b = 0x1buy && (char buffer[2] = '[' || char buffer[2] = 'O') then
                    // The second ESC is not a keypress. It is the start of the
                    // next sequence, which arrives straight after Escape. Emit
                    // a plain Escape for the first byte. The next step reads
                    // what follows.
                    Emitted([ key Key.Esc ], 1)
                elif b = 0x1buy then
                    // Make the same control-or-text choice as step. Do not call
                    // step itself. It would recurse on a second ESC and not
                    // treat it as Alt+Esc.
                    withAlt (Emitted([ key Key.Esc ], 1))
                elif b < 0x20uy || b = 0x7fuy then
                    withAlt (control b)
                else
                    withAlt (text (buffer.Slice 1))

    let private step (buffer: ReadOnlySpan<byte>) =
        let b = buffer[0]

        if b = 0x1buy then escape buffer
        elif b < 0x20uy || b = 0x7fuy then control b
        else text buffer

    let private run (buffer: ReadOnlySpan<byte>) =
        let events = ResizeArray<InputEvent>()
        let mutable pos = 0
        let mutable stop = false

        while not stop && pos < buffer.Length do
            match step (buffer.Slice pos) with
            | Emitted(es, n) ->
                events.AddRange es
                pos <- pos + n
            | Skipped n -> pos <- pos + n
            | Incomplete -> stop <- true

        List.ofSeq events, pos

    /// Reads as many complete events as the buffer holds. Anything left is a
    /// sequence that has not finished arriving. The caller keeps those bytes
    /// and calls again when the terminal sends more.
    let parse (buffer: ReadOnlySpan<byte>) : InputEvent list * int = run buffer

    /// Same as parse, but for a buffer that the caller knows will get no more
    /// bytes. A trailing lone ESC is then the Escape key, not the start of a
    /// sequence. The parser still leaves a longer unfinished sequence alone.
    /// Waiting for it is correct. Making a key from it is not.
    let parseFinal (buffer: ReadOnlySpan<byte>) : InputEvent list * int =
        let events, consumed = run buffer

        if consumed = buffer.Length - 1 && buffer[consumed] = 0x1buy then
            events @ [ key Key.Esc ], buffer.Length
        elif
            consumed = buffer.Length - 2
            && buffer[consumed] = 0x1buy
            && buffer[consumed + 1] = 0x1buy
        then
            // Nothing arrived to show what the second ESC started. So it
            // started nothing. Treat it as Escape with Alt, because two ESC
            // bytes arrived.
            let altEscape =
                InputEvent.Key
                    { Key = Key.Esc
                      Ctrl = false
                      Alt = true
                      Shift = false }

            events @ [ altEscape ], buffer.Length
        elif
            consumed = buffer.Length - 2
            && buffer[consumed] = 0x1buy
            && (buffer[consumed + 1] = byte '[' || buffer[consumed + 1] = byte 'O')
        then
            // Nothing followed, so no sequence started. The terminal sends
            // Alt+key as ESC then the key. These two keys are also the bytes
            // that open a sequence.
            events @ [ InputEvent.Key(KeyEvent.alt (Key.Char(char buffer[consumed + 1]))) ], buffer.Length
        else
            events, consumed
