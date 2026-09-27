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

    /// How many bytes this UTF-8 lead byte starts a character with, or 0 if it is
    /// not a lead byte.
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
        // Terminals disagree about which byte Ctrl+Backspace sends, so keep both
        // reachable rather than folding them together.
        | 0x08uy -> Emitted([ ctrlKey Key.Backspace ], 1)
        | b when b >= 0x01uy && b <= 0x1auy -> Emitted([ ctrlKey (Key.Char(char (b + 96uy))) ], 1)
        | _ -> Skipped 1

    let private text (buffer: ReadOnlySpan<byte>) =
        let width = charLength buffer[0]

        if width = 0 then
            // A continuation byte with no lead. Drop it rather than stalling.
            Skipped 1
        elif width > buffer.Length then
            Incomplete
        else
            let decoded = Encoding.UTF8.GetString(buffer.Slice(0, width))
            // An astral character decodes to a UTF-16 surrogate pair. Emit one
            // Key.Char per code unit rather than dropping the second half; a
            // consumer reassembles the pair by appending.
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

    /// Internal so the reader can tell a stalled paste from a malformed tail, and
    /// so it can find the end of a paste it has already started handing over.
    [<Literal>]
    let internal PasteStart = "\x1b[200~"

    [<Literal>]
    let internal PasteEnd = "\x1b[201~"

    let private PasteStartBytes = Encoding.ASCII.GetBytes PasteStart
    let private PasteEndBytes = Encoding.ASCII.GetBytes PasteEnd

    /// SGR mouse: ESC [ < button ; x ; y then M for press or m for release.
    /// Coordinates arrive one-based and are reported zero-based, to match Rect.
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

    /// Everything between the paste markers is text, including bytes that would
    /// otherwise look like escape sequences. Without the terminator in hand the
    /// whole paste waits, so a paste bigger than one read still arrives whole.
    /// Finds the terminator in the raw bytes rather than decoding the whole
    /// buffer up front: a paste split across many reads would otherwise get
    /// re-decoded in full on every call, and a body that is not valid UTF-8
    /// would throw the byte count off once it round-trips through GetString and
    /// GetByteCount.
    let private paste (buffer: ReadOnlySpan<byte>) =
        let bodyStart = PasteStartBytes.Length
        let endAt = buffer.Slice(bodyStart).IndexOf(ReadOnlySpan PasteEndBytes)

        if endAt < 0 then
            Incomplete
        else
            let body = Encoding.UTF8.GetString(buffer.Slice(bodyStart, endAt))
            let consumed = bodyStart + endAt + PasteEndBytes.Length
            Emitted([ InputEvent.Paste body ], consumed)

    /// A CSI sequence is ESC [ then digits and semicolons, then one final byte in
    /// the range 0x40 to 0x7e. Waiting for that final byte is what tells a sequence
    /// that has not finished arriving apart from one that has.
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

            while finalAt < 0 && i < buffer.Length do
                let b = buffer[i]

                if b >= 0x40uy && b <= 0x7euy then
                    finalAt <- i
                else
                    i <- i + 1

            if finalAt < 0 then
                Incomplete
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
            | _ -> Skipped 3

    /// Applies Alt to every key in a step's result, and accounts for the ESC byte
    /// that step didn't see.
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
            // Could be the Escape key, could be the first byte of a sequence. Only
            // time tells them apart, and that is parseFinal's job.
            Incomplete
        else
            match char buffer[1] with
            | '[' -> csi buffer
            | 'O' -> ss3 buffer
            | _ ->
                // Same control-or-text choice step makes, but not step itself: that
                // would recurse on a second ESC instead of treating it as Alt+Esc.
                let b = buffer[1]

                if b = 0x1buy then withAlt (Emitted([ key Key.Esc ], 1))
                elif b < 0x20uy || b = 0x7fuy then withAlt (control b)
                else withAlt (text (buffer.Slice 1))

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

    /// Read as many complete events as the buffer holds. Anything left over is a
    /// sequence that has not finished arriving, so the caller keeps those bytes and
    /// calls again once the terminal sends more.
    let parse (buffer: ReadOnlySpan<byte>) : InputEvent list * int = run buffer

    /// Same as parse, but for a buffer the caller has decided will get no more
    /// bytes. A trailing lone ESC is then the Escape key rather than the start of
    /// something. A longer unfinished sequence is still left alone — waiting on it
    /// is right, inventing a key from it is not.
    let parseFinal (buffer: ReadOnlySpan<byte>) : InputEvent list * int =
        let events, consumed = run buffer

        if consumed = buffer.Length - 1 && buffer[consumed] = 0x1buy then
            events @ [ key Key.Esc ], buffer.Length
        else
            events, consumed
