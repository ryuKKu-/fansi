module Fansi.Tests.InputParserTests

open System
open System.Text
open Xunit
open Fansi.Core

[<Fact>]
let ``a plain key event carries no modifiers`` () =
    let e = KeyEvent.plain Key.Enter
    Assert.Equal(Key.Enter, e.Key)
    Assert.False(e.Ctrl)
    Assert.False(e.Alt)
    Assert.False(e.Shift)

let private parse (s: string) =
    InputParser.parse (ReadOnlySpan(Encoding.UTF8.GetBytes s))

let private parseBytes (bytes: byte list) =
    InputParser.parse (ReadOnlySpan(Array.ofList bytes))

let private parseFinal (s: string) =
    InputParser.parseFinal (ReadOnlySpan(Encoding.UTF8.GetBytes s))

[<Fact>]
let ``plain letters come through as characters`` () =
    let events, consumed = parse "abc"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key(KeyEvent.plain (Key.Char 'a'))
          InputEvent.Key(KeyEvent.plain (Key.Char 'b'))
          InputEvent.Key(KeyEvent.plain (Key.Char 'c')) ],
        events
    )

    Assert.Equal(3, consumed)

[<Fact>]
let ``a multi-byte character is one event`` () =
    // the euro sign is three bytes
    let events, consumed = parse "€"
    Assert.Equal<InputEvent list>([ InputEvent.Key(KeyEvent.plain (Key.Char '€')) ], events)
    Assert.Equal(3, consumed)

[<Fact>]
let ``a character split across reads is left in the buffer`` () =
    let full = Encoding.UTF8.GetBytes "€"
    let events, consumed = parseBytes [ full[0]; full[1] ]
    Assert.Empty(events)
    Assert.Equal(0, consumed)

[<Fact>]
let ``a 4-byte character produces one event per utf-16 code unit`` () =
    // an emoji is 4 bytes and decodes to a UTF-16 surrogate pair;
    // a consumer reassembles the pair by appending the two characters
    let events, consumed = parse "😀"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key(KeyEvent.plain (Key.Char '\uD83D'))
          InputEvent.Key(KeyEvent.plain (Key.Char '\uDE00')) ],
        events
    )

    Assert.Equal(4, consumed)

[<Fact>]
let ``an orphan continuation byte is dropped`` () =
    let events, consumed = parseBytes [ 0x80uy ]
    Assert.Empty(events)
    Assert.Equal(1, consumed)

[<Fact>]
let ``control bytes map to their own keys`` () =
    let events, consumed = parseBytes [ 0x0duy; 0x09uy; 0x7fuy ]

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key(KeyEvent.plain Key.Enter)
          InputEvent.Key(KeyEvent.plain Key.Tab)
          InputEvent.Key(KeyEvent.plain Key.Backspace) ],
        events
    )

    Assert.Equal(3, consumed)

[<Fact>]
let ``a control letter arrives as the letter with ctrl set`` () =
    let events, _ = parseBytes [ 0x03uy ]

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key
              { Key = Key.Char 'c'
                Ctrl = true
                Alt = false
                Shift = false } ],
        events
    )

[<Fact>]
let ``backspace sent as 0x08 carries ctrl`` () =
    let events, _ = parseBytes [ 0x08uy ]

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key
              { Key = Key.Backspace
                Ctrl = true
                Alt = false
                Shift = false } ],
        events
    )

[<Fact>]
let ``arrow keys decode`` () =
    let events, consumed = parse "\x1b[A\x1b[B\x1b[C\x1b[D"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key(KeyEvent.plain Key.Up)
          InputEvent.Key(KeyEvent.plain Key.Down)
          InputEvent.Key(KeyEvent.plain Key.Right)
          InputEvent.Key(KeyEvent.plain Key.Left) ],
        events
    )

    Assert.Equal(12, consumed)

[<Fact>]
let ``tilde sequences decode`` () =
    let events, _ = parse "\x1b[2~\x1b[3~\x1b[5~\x1b[6~"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key(KeyEvent.plain Key.Insert)
          InputEvent.Key(KeyEvent.plain Key.Delete)
          InputEvent.Key(KeyEvent.plain Key.PageUp)
          InputEvent.Key(KeyEvent.plain Key.PageDown) ],
        events
    )

[<Fact>]
let ``function keys decode from both forms`` () =
    let events, _ = parse "\x1bOP\x1bOS\x1b[15~\x1b[24~"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key(KeyEvent.plain (Key.F 1))
          InputEvent.Key(KeyEvent.plain (Key.F 4))
          InputEvent.Key(KeyEvent.plain (Key.F 5))
          InputEvent.Key(KeyEvent.plain (Key.F 12)) ],
        events
    )

[<Fact>]
let ``a modifier parameter sets the flags`` () =
    let events, _ = parse "\x1b[1;5A\x1b[1;2C\x1b[1;3B"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key
              { Key = Key.Up
                Ctrl = true
                Alt = false
                Shift = false }
          InputEvent.Key
              { Key = Key.Right
                Ctrl = false
                Alt = false
                Shift = true }
          InputEvent.Key
              { Key = Key.Down
                Ctrl = false
                Alt = true
                Shift = false } ],
        events
    )

[<Fact>]
let ``escape then a letter is alt`` () =
    let events, _ = parse "\x1bf"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key
              { Key = Key.Char 'f'
                Ctrl = false
                Alt = true
                Shift = false } ],
        events
    )

[<Fact>]
let ``alt plus an astral character keeps both code units`` () =
    let events, consumed = parse "\x1b😀"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key
              { Key = Key.Char '\uD83D'
                Ctrl = false
                Alt = true
                Shift = false }
          InputEvent.Key
              { Key = Key.Char '\uDE00'
                Ctrl = false
                Alt = true
                Shift = false } ],
        events
    )

    Assert.Equal(5, consumed)

[<Fact>]
let ``escape then a control byte is alt on that key, not a character`` () =
    let events, _ = parseBytes [ 0x1buy; 0x0duy ]

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key
              { Key = Key.Enter
                Ctrl = false
                Alt = true
                Shift = false } ],
        events
    )

[<Fact>]
let ``two escapes in one read is alt-escape, not a raw control character`` () =
    let events, consumed = parseBytes [ 0x1buy; 0x1buy ]

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key
              { Key = Key.Esc
                Ctrl = false
                Alt = true
                Shift = false } ],
        events
    )

    Assert.Equal(2, consumed)

[<Fact>]
let ``escape then an orphan continuation byte is dropped, both bytes consumed`` () =
    let events, consumed = parseBytes [ 0x1buy; 0x80uy ]
    Assert.Empty(events)
    Assert.Equal(2, consumed)

[<Fact>]
let ``a sequence split across reads is left in the buffer`` () =
    // an arrow key arriving in two chunks must not be read as Escape and a bracket
    let events, consumed = parse "ab\x1b["

    Assert.Equal<InputEvent list>(
        [ InputEvent.Key(KeyEvent.plain (Key.Char 'a'))
          InputEvent.Key(KeyEvent.plain (Key.Char 'b')) ],
        events
    )

    Assert.Equal(2, consumed)

[<Fact>]
let ``a lone escape waits for parse and arrives for parseFinal`` () =
    let waiting, consumed = parse "\x1b"
    Assert.Empty(waiting)
    Assert.Equal(0, consumed)

    let settled, consumedFinal = parseFinal "\x1b"
    Assert.Equal<InputEvent list>([ InputEvent.Key(KeyEvent.plain Key.Esc) ], settled)
    Assert.Equal(1, consumedFinal)

[<Fact>]
let ``parseFinal leaves a genuinely partial sequence alone`` () =
    // ESC [ 1 ; with no final byte is not Escape, it is a truncated sequence
    let events, consumed = parseFinal "\x1b[1;"
    Assert.Empty(events)
    Assert.Equal(0, consumed)

[<Fact>]
let ``an sgr mouse press decodes with zero-based coordinates`` () =
    let events, _ = parse "\x1b[<0;10;5M"

    Assert.Equal<InputEvent list>(
        [ InputEvent.Mouse
              { Button = MouseButton.Left
                Action = MouseAction.Press
                X = 9
                Y = 4
                Ctrl = false
                Alt = false
                Shift = false } ],
        events
    )

[<Fact>]
let ``a lowercase terminator is a release`` () =
    let events, _ = parse "\x1b[<0;1;1m"

    match events with
    | [ InputEvent.Mouse m ] -> Assert.Equal(MouseAction.Release, m.Action)
    | other -> failwith $"expected one mouse event, got %A{other}"

[<Fact>]
let ``mouse modifiers and wheel decode`` () =
    // 64 is wheel up, +16 ctrl
    let events, _ = parse "\x1b[<80;1;1M"

    match events with
    | [ InputEvent.Mouse m ] ->
        Assert.Equal(MouseButton.ScrollUp, m.Button)
        Assert.True(m.Ctrl)
    | other -> failwith $"expected one mouse event, got %A{other}"

[<Fact>]
let ``a motion event decodes as move`` () =
    // 32 is the motion bit
    let events, _ = parse "\x1b[<32;3;3M"

    match events with
    | [ InputEvent.Mouse m ] -> Assert.Equal(MouseAction.Move, m.Action)
    | other -> failwith $"expected one mouse event, got %A{other}"

[<Fact>]
let ``a bracketed paste is one event`` () =
    let events, consumed = parse "\x1b[200~hello world\x1b[201~"
    Assert.Equal<InputEvent list>([ InputEvent.Paste "hello world" ], events)
    Assert.Equal(23, consumed)

[<Fact>]
let ``a paste containing an escape sequence stays literal`` () =
    let events, _ = parse "\x1b[200~a\x1b[Ab\x1b[201~"
    Assert.Equal<InputEvent list>([ InputEvent.Paste "a\x1b[Ab" ], events)

[<Fact>]
let ``an unfinished paste waits for the rest`` () =
    // the terminator has not arrived, so nothing is consumed and nothing is lost
    let events, consumed = parse "\x1b[200~half a pa"
    Assert.Empty(events)
    Assert.Equal(0, consumed)

[<Fact>]
let ``a paste body that is not valid utf-8 still consumes exactly the whole paste`` () =
    // a lone continuation byte has no valid decoding; the consumed count must
    // still come from the raw bytes, not from re-encoding the (lossy) decoded string
    let start = Encoding.ASCII.GetBytes "\x1b[200~" |> List.ofArray
    let stop = Encoding.ASCII.GetBytes "\x1b[201~" |> List.ofArray
    let bytes = start @ [ 0x80uy ] @ stop

    let _, consumed = parseBytes bytes
    Assert.Equal(bytes.Length, consumed)

[<Fact>]
let ``focus in and out decode`` () =
    let events, _ = parse "\x1b[I\x1b[O"
    Assert.Equal<InputEvent list>([ InputEvent.FocusChanged true; InputEvent.FocusChanged false ], events)
