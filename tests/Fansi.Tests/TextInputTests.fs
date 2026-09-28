module Fansi.Tests.TextInputTests

open Xunit
open Fansi
open Fansi.Core

module T = TextInputComponent

let private fresh () = fst (T.init ())

let private send msg model = fst (T.update msg model)

let private press key model = send (T.KeyInput key) model

let private typeText (s: string) model =
    s |> Seq.fold (fun m c -> press (KeyEvent.plain (Key.Char c)) m) model

let private withValue s model = send (T.SetValue s) model

let private line width focused model =
    (Paint.render width 1 (T.view focused model) |> Buffer.toLines).Head.TrimEnd()

let private left = KeyEvent.plain Key.Left
let private right = KeyEvent.plain Key.Right
let private backspace = KeyEvent.plain Key.Backspace
let private delete = KeyEvent.plain Key.Delete

// --- identity and bugs from the old component ---

[<Fact>]
let ``two text inputs get their own cursor id`` () =
    Assert.NotEqual((fresh ()).Cursor.Id, (fresh ()).Cursor.Id)

[<Fact>]
let ``an old model does not see a newer model's text`` () =
    let before = fresh ()
    let _after = typeText "abc" before
    Assert.Equal("", before.Value)

[<Fact>]
let ``the view picks no border of its own`` () =
    let lines =
        Paint.render 10 3 (T.view false (withValue "hi" (fresh ()))) |> Buffer.toLines

    Assert.Equal("> hi", lines[0].TrimEnd())
    Assert.Equal("", lines[1].Trim())

// --- typing ---

[<Fact>]
let ``the text input shows its prompt and its value`` () =
    Assert.StartsWith("> hi", line 10 false (withValue "hi" (fresh ())))

[<Fact>]
let ``a plain character is inserted`` () =
    Assert.Equal("a", (press (KeyEvent.plain (Key.Char 'a')) (fresh ())).Value)

[<Fact>]
let ``ctrl+letter does not insert`` () =
    Assert.Equal("", (press (KeyEvent.ctrl (Key.Char 'q')) (fresh ())).Value)

[<Fact>]
let ``alt+letter does not insert`` () =
    Assert.Equal("", (press (KeyEvent.alt (Key.Char 'q')) (fresh ())).Value)

[<Fact>]
let ``characters go in at the cursor`` () =
    let m = typeText "ac" (fresh ()) |> press left |> typeText "b"
    Assert.Equal("abc", m.Value)
    Assert.Equal(2, m.Pos)

[<Fact>]
let ``enter and tab do not insert`` () =
    let m =
        fresh () |> press (KeyEvent.plain Key.Enter) |> press (KeyEvent.plain Key.Tab)

    Assert.Equal("", m.Value)

// --- CharLimit ---

[<Fact>]
let ``typing stops at the character limit`` () =
    let m = { (fresh ()) with CharLimit = 3 } |> typeText "abcde"
    Assert.Equal("abc", m.Value)

[<Fact>]
let ``a paste that fits the limit goes in whole`` () =
    let m = { (fresh ()) with CharLimit = 10 } |> send (T.Pasted "abcdefgh")
    Assert.Equal("abcdefgh", m.Value)

[<Fact>]
let ``a paste is cut to the room that is left`` () =
    let m =
        { (fresh ()) with CharLimit = 10 }
        |> send (T.Pasted "abcde")
        |> send (T.Pasted "fghijklmn")

    Assert.Equal("abcdefghij", m.Value)

[<Fact>]
let ``a paste inserts nothing once the limit is reached`` () =
    let m =
        { (fresh ()) with CharLimit = 3 }
        |> send (T.Pasted "abc")
        |> send (T.Pasted "de")

    Assert.Equal("abc", m.Value)

[<Fact>]
let ``a limit of zero means no limit`` () =
    let m = fresh () |> send (T.Pasted "a longer string than any limit")
    Assert.Equal("a longer string than any limit", m.Value)

[<Fact>]
let ``set value respects the limit`` () =
    let m = { (fresh ()) with CharLimit = 4 } |> withValue "abcdef"
    Assert.Equal("abcd", m.Value)
    Assert.Equal(4, m.Pos)

// --- paste ---

[<Fact>]
let ``a paste goes in at the cursor and moves it past the text`` () =
    let m = typeText "ad" (fresh ()) |> press left |> send (T.Pasted "bc")
    Assert.Equal("abcd", m.Value)
    Assert.Equal(3, m.Pos)

[<Fact>]
let ``a paste drops control characters so the value stays on one line`` () =
    let m = fresh () |> send (T.Pasted "one\r\ntwo\tthree\u001b")
    Assert.Equal("onetwothree", m.Value)

[<Fact>]
let ``set value drops control characters too`` () =
    let m = fresh () |> withValue "one\ttwo\nthree"
    Assert.Equal("onetwothree", m.Value)
    Assert.Equal(11, m.Pos)

// --- surrogate pairs (Review Focus 2) ---

let private wave = "\U0001F44B"

[<Fact>]
let ``an emoji typed as two halves goes in whole`` () =
    let m = typeText ("a" + wave) (fresh ())
    Assert.Equal("a" + wave, m.Value)
    Assert.Equal(3, m.Pos)

[<Fact>]
let ``the limit never keeps half an emoji`` () =
    let typed = { (fresh ()) with CharLimit = 2 } |> typeText ("a" + wave)
    Assert.Equal("a", typed.Value)

    let pasted = { (fresh ()) with CharLimit = 2 } |> send (T.Pasted("a" + wave))
    Assert.Equal("a", pasted.Value)

[<Fact>]
let ``left and right step over an emoji in one press`` () =
    let m = withValue ("a" + wave + "b") (fresh ())
    let m = press left m
    Assert.Equal(3, m.Pos)
    let m = press left m
    Assert.Equal(1, m.Pos)
    let m = press right m
    Assert.Equal(3, m.Pos)

[<Fact>]
let ``backspace and delete remove a whole emoji`` () =
    let back = withValue ("a" + wave) (fresh ()) |> press backspace
    Assert.Equal("a", back.Value)

    let forward =
        withValue ("a" + wave) (fresh ())
        |> press (KeyEvent.plain Key.Home)
        |> press right
        |> press delete

    Assert.Equal("a", forward.Value)

[<Fact>]
let ``set value drops a lone high surrogate`` () =
    let m = fresh () |> withValue ("ab" + string '\uD83D')
    Assert.Equal("ab", m.Value)
    Assert.Equal(2, m.Pos)

// --- motion and deletion at the boundaries ---

[<Fact>]
let ``left at the start and right at the end stay put`` () =
    let m = withValue "ab" (fresh ())
    Assert.Equal(2, (press right m).Pos)
    Assert.Equal(0, (m |> press (KeyEvent.plain Key.Home) |> press left).Pos)

[<Fact>]
let ``backspace at the start and delete at the end change nothing`` () =
    let m = withValue "ab" (fresh ())
    Assert.Equal("ab", (press delete m).Value)
    Assert.Equal("ab", (m |> press (KeyEvent.plain Key.Home) |> press backspace).Value)

[<Fact>]
let ``backspace and delete in the middle`` () =
    let m = withValue "abc" (fresh ()) |> press left
    let back = press backspace m
    Assert.Equal("ac", back.Value)
    Assert.Equal(1, back.Pos)
    let forward = press delete m
    Assert.Equal("ab", forward.Value)
    Assert.Equal(2, forward.Pos)

[<Fact>]
let ``home and end and their ctrl bindings`` () =
    let m = withValue "abc" (fresh ())
    Assert.Equal(0, (press (KeyEvent.plain Key.Home) m).Pos)
    Assert.Equal(0, (press (KeyEvent.ctrl (Key.Char 'a')) m).Pos)
    let atStart = press (KeyEvent.plain Key.Home) m
    Assert.Equal(3, (press (KeyEvent.plain Key.End) atStart).Pos)
    Assert.Equal(3, (press (KeyEvent.ctrl (Key.Char 'e')) atStart).Pos)

[<Fact>]
let ``ctrl+f and ctrl+b move by one character`` () =
    let m = withValue "abc" (fresh ())
    Assert.Equal(2, (press (KeyEvent.ctrl (Key.Char 'b')) m).Pos)
    Assert.Equal(3, (m |> press left |> press (KeyEvent.ctrl (Key.Char 'f'))).Pos)

// --- words ---

[<Fact>]
let ``word motion skips whitespace then the word`` () =
    let m = withValue "one  two three" (fresh ()) |> press (KeyEvent.plain Key.Home)
    let m = press (KeyEvent.ctrl Key.Right) m
    Assert.Equal(3, m.Pos)
    let m = press (KeyEvent.alt (Key.Char 'f')) m
    Assert.Equal(8, m.Pos)
    let m = press (KeyEvent.ctrl Key.Left) m
    Assert.Equal(5, m.Pos)
    let m = press (KeyEvent.alt (Key.Char 'b')) m
    Assert.Equal(0, m.Pos)

[<Fact>]
let ``word motion over mixed whitespace`` () =
    // "a, space, no-break space, space, b": the word "b" starts at 4, "a" at 0.
    let m = withValue "a \u00a0 b" (fresh ())
    let once = press (KeyEvent.ctrl Key.Left) m
    Assert.Equal(4, once.Pos)
    Assert.Equal(0, (press (KeyEvent.ctrl Key.Left) once).Pos)
    Assert.Equal(1, (press (KeyEvent.ctrl Key.Right) (press (KeyEvent.plain Key.Home) m)).Pos)

[<Fact>]
let ``delete word backward and forward`` () =
    let m = withValue "one two three" (fresh ()) |> press (KeyEvent.ctrl Key.Left)
    let back = press (KeyEvent.ctrl Key.Backspace) m
    Assert.Equal("one three", back.Value)
    Assert.Equal(4, back.Pos)

    let forward = press (KeyEvent.alt (Key.Char 'd')) m
    Assert.Equal("one two ", forward.Value)
    Assert.Equal(8, forward.Pos)

[<Fact>]
let ``ctrl+w and alt+backspace also delete a word backward`` () =
    let m = withValue "one two" (fresh ())
    Assert.Equal("one ", (press (KeyEvent.ctrl (Key.Char 'w')) m).Value)
    Assert.Equal("one ", (press (KeyEvent.alt Key.Backspace) m).Value)

[<Fact>]
let ``delete to line end and line start`` () =
    let m = withValue "abcd" (fresh ()) |> press left |> press left
    Assert.Equal("ab", (press (KeyEvent.ctrl (Key.Char 'k')) m).Value)
    let toStart = press (KeyEvent.ctrl (Key.Char 'u')) m
    Assert.Equal("cd", toStart.Value)
    Assert.Equal(0, toStart.Pos)

// --- scrolling (Review Focus 3) ---

[<Fact>]
let ``with no width the view starts at the first character`` () =
    let m = withValue "abcdefgh" (fresh ())
    Assert.Equal(0, m.Offset)

[<Fact>]
let ``typing past the width scrolls and keeps the cursor on screen`` () =
    let m = { (fresh ()) with Width = 5 } |> typeText "abcdefgh"
    Assert.Equal(4, m.Offset)
    Assert.Equal("> efgh", line 12 false m)

[<Fact>]
let ``home scrolls back to the start and end scrolls to the end`` () =
    let m = { (fresh ()) with Width = 5 } |> typeText "abcdefgh"
    let home = press (KeyEvent.plain Key.Home) m
    Assert.Equal(0, home.Offset)
    Assert.Equal("> abcde", line 12 false home)
    Assert.Equal(4, (press (KeyEvent.plain Key.End) home).Offset)

[<Fact>]
let ``moving right against the edge scrolls by one`` () =
    let m =
        { (fresh ()) with Width = 5 }
        |> typeText "abcdefgh"
        |> press (KeyEvent.plain Key.Home)

    let m = [ 1..5 ] |> List.fold (fun m _ -> press right m) m
    Assert.Equal(5, m.Pos)
    Assert.Equal(1, m.Offset)
    Assert.Equal("> bcdef", line 12 false m)

[<Fact>]
let ``deleting pulls the offset back when the value gets shorter`` () =
    let m = { (fresh ()) with Width = 5 } |> typeText "abcdefgh"
    let m = [ 1..6 ] |> List.fold (fun m _ -> press backspace m) m
    Assert.Equal("ab", m.Value)
    Assert.Equal(0, m.Offset)

// --- the view against direct model edits (Review Focus: I1, M1, M2) ---

[<Fact>]
let ``setting Width directly still scrolls once view runs`` () =
    let m =
        { withValue "abcdefgh" (fresh ()) with
            Width = 5 }

    Assert.Equal("> efgh", line 12 false m)

[<Fact>]
let ``a Pos set past the end is clamped instead of throwing`` () =
    let m =
        { withValue "ab" (fresh ()) with
            Pos = 10 }

    Assert.Equal("> ab", line 12 false m)

[<Fact>]
let ``the right edge never splits an emoji`` () =
    let m =
        { (fresh ()) with Width = 5 }
        |> withValue ("abcd" + wave)
        |> press (KeyEvent.plain Key.Home)

    Assert.Equal("> abcd", line 12 false m)

[<Fact>]
let ``the offset steps past a low surrogate at the edge`` () =
    let m = { (fresh ()) with Width = 2 } |> withValue ("abc" + wave)
    Assert.Equal(5, m.Offset)

// --- update against a hand-written model (I1) ---

[<Fact>]
let ``typing after a hand-set Value fixes a stale Pos instead of throwing`` () =
    let m =
        { (typeText "hello" (fresh ())) with
            Value = "" }
        |> typeText "a"

    Assert.Equal("a", m.Value)
    Assert.Equal(1, m.Pos)

[<Fact>]
let ``backspace after a hand-set Value fixes a stale Pos instead of throwing`` () =
    let m =
        { (typeText "hello" (fresh ())) with
            Value = "" }
        |> press backspace

    Assert.Equal("", m.Value)
    Assert.Equal(0, m.Pos)

[<Fact>]
let ``paste after a hand-set Value fixes a stale Pos instead of throwing`` () =
    let m =
        { (typeText "hello" (fresh ())) with
            Value = "" }
        |> send (T.Pasted "xy")

    Assert.Equal("xy", m.Value)
    Assert.Equal(2, m.Pos)

[<Fact>]
let ``backspace with Pos hand-set between the halves of an emoji removes the whole emoji`` () =
    let m =
        { withValue ("a" + wave) (fresh ()) with
            Pos = 2 }
        |> press backspace

    Assert.Equal("a", m.Value)

[<Fact>]
let ``error catches up on the first update after a direct Value write`` () =
    let required s =
        if s = "" then T.Invalid "required" else T.Valid

    let m =
        { (fresh () |> T.withValidation required |> typeText "a") with
            Value = "" }
        |> press left

    Assert.Equal(Some "required", m.Error)

// --- echo, placeholder ---

[<Fact>]
let ``password echo shows the mask and never touches the value`` () =
    let m =
        { (fresh ()) with
            Echo = T.Password '*' }
        |> typeText "secret"

    Assert.Equal("secret", m.Value)
    Assert.Equal("> ******", line 12 false m)

[<Fact>]
let ``password echo masks an emoji as one character`` () =
    let m =
        { (fresh ()) with
            Echo = T.Password '*' }
        |> typeText ("a" + wave)

    Assert.Equal("> **", line 12 false m)

[<Fact>]
let ``hidden echo shows nothing and never touches the value`` () =
    let m = { (fresh ()) with Echo = T.Hidden } |> typeText "secret"
    Assert.Equal("secret", m.Value)
    Assert.Equal(">", line 12 false m)

[<Fact>]
let ``the placeholder shows in its own style while the value is empty`` () =
    let m = { (fresh ()) with Placeholder = "name" }
    Assert.Equal("> name", line 12 false m)
    let buffer = Paint.render 12 1 (T.view false m)
    Assert.Equal(m.PlaceholderStyle, (Buffer.get buffer 3 0).Style)

[<Fact>]
let ``the placeholder goes once there is a value`` () =
    let m = { (fresh ()) with Placeholder = "name" } |> typeText "x"
    Assert.Equal("> x", line 12 false m)

// --- the cursor in the view ---

[<Fact>]
let ``the cursor shows only when the input is focused`` () =
    let m = withValue "ab" (fresh ()) |> press left
    let focused = Paint.render 12 1 (T.view true m)
    let unfocused = Paint.render 12 1 (T.view false m)
    Assert.Equal(Color.Cyan, (Buffer.get focused 3 0).Style.BgColor)
    Assert.Equal(Color.Default, (Buffer.get unfocused 3 0).Style.BgColor)

[<Fact>]
let ``only the focused input subscribes to the blink`` () =
    let m = fresh ()
    Assert.Equal<string list list>([ [ "fansi"; "cursor"; string m.Cursor.Id ] ], T.subscribe true m |> List.map fst)
    Assert.Empty(T.subscribe false m)

[<Fact>]
let ``a cursor message reaches the cursor`` () =
    let m = fresh ()
    let ticked = send (T.CursorMsg(Cursor.BlinkTick m.Cursor.Id)) m
    Assert.Equal(not m.Cursor.Blink, ticked.Cursor.Blink)

// --- undo ---

let private undo = KeyEvent.ctrl (Key.Char 'z')
let private redo = KeyEvent.ctrl (Key.Char 'y')

[<Fact>]
let ``undo takes back a typed word in one step`` () =
    let m = typeText "hello" (fresh ()) |> press undo
    Assert.Equal("", m.Value)
    Assert.Equal(0, m.Pos)

[<Fact>]
let ``redo puts it back`` () =
    let m = typeText "hello" (fresh ()) |> press undo |> press redo
    Assert.Equal("hello", m.Value)
    Assert.Equal(5, m.Pos)

[<Fact>]
let ``a cursor move ends the typing step`` () =
    let m = typeText "ab" (fresh ()) |> press left |> press right |> typeText "cd"
    let once = press undo m
    Assert.Equal("ab", once.Value)
    Assert.Equal("", (press undo once).Value)

[<Fact>]
let ``each delete is its own step`` () =
    let m = typeText "abc" (fresh ()) |> press backspace |> press backspace
    let once = press undo m
    Assert.Equal("ab", once.Value)
    Assert.Equal("abc", (press undo once).Value)

[<Fact>]
let ``a paste is one step and ends the typing step`` () =
    let m = typeText "ab" (fresh ()) |> send (T.Pasted "XYZ") |> typeText "c"
    let once = press undo m
    Assert.Equal("abXYZ", once.Value)
    let twice = press undo once
    Assert.Equal("ab", twice.Value)
    Assert.Equal("", (press undo twice).Value)

[<Fact>]
let ``a paste that inserts nothing still ends the typing step`` () =
    let m = typeText "ab" (fresh ()) |> send (T.Pasted "\r\n") |> typeText "cd"
    Assert.Equal("ab", (press undo m).Value)

[<Fact>]
let ``set value is its own step`` () =
    let m = typeText "ab" (fresh ()) |> withValue "new"
    let back = press undo m
    Assert.Equal("ab", back.Value)
    Assert.Equal(2, back.Pos)

[<Fact>]
let ``an edit that changes nothing adds no step`` () =
    let m = typeText "ab" (fresh ()) |> press delete |> press delete
    Assert.Equal("", (press undo m).Value)

    let atStart =
        withValue "ab" (fresh ()) |> press (KeyEvent.plain Key.Home) |> press backspace

    Assert.Equal("", (press undo atStart).Value)

[<Fact>]
let ``a new edit clears redo`` () =
    let m = typeText "ab" (fresh ()) |> press undo |> typeText "x" |> press redo
    Assert.Equal("x", m.Value)

[<Fact>]
let ``undo and redo with nothing to do change nothing`` () =
    let m = withValue "ab" (fresh ())

    let m =
        { m with
            History =
                { Past = []
                  Future = []
                  Typing = false } }

    Assert.Equal("ab", (press undo m).Value)
    Assert.Equal("ab", (press redo m).Value)

[<Fact>]
let ``undo depth is capped at 100`` () =
    let m = [ 1..150 ] |> List.fold (fun m i -> withValue (string i) m) (fresh ())
    Assert.Equal(100, m.History.Past.Length)
    let all = [ 1..150 ] |> List.fold (fun m _ -> press undo m) m
    Assert.Equal("50", all.Value)

[<Fact>]
let ``undo scrolls to where the cursor comes back`` () =
    let m =
        { (fresh ()) with Width = 5 }
        |> typeText "abcdefgh"
        |> press (KeyEvent.ctrl (Key.Char 'u'))

    Assert.Equal(0, m.Offset)
    let back = press undo m
    Assert.Equal("abcdefgh", back.Value)
    Assert.Equal(8, back.Pos)
    Assert.Equal(4, back.Offset)

[<Fact>]
let ``an old model keeps its own history`` () =
    let before = typeText "ab" (fresh ())
    let _after = press backspace before
    Assert.Equal("", (press undo before).Value)

// --- validation ---

let private required s =
    if s = "" then T.Invalid "required" else T.Valid

[<Fact>]
let ``by default every value is valid`` () =
    Assert.Equal(None, (typeText "x" (fresh ())).Error)

[<Fact>]
let ``validation runs on every change`` () =
    let m = fresh () |> T.withValidation required
    Assert.Equal(Some "required", m.Error)
    let typed = typeText "a" m
    Assert.Equal(None, typed.Error)
    Assert.Equal(Some "required", (press backspace typed).Error)

[<Fact>]
let ``validation runs after undo and redo`` () =
    let m = fresh () |> T.withValidation required |> typeText "a"
    let undone = press undo m
    Assert.Equal(Some "required", undone.Error)
    Assert.Equal(None, (press redo undone).Error)

[<Fact>]
let ``an invalid value renders in the error style`` () =
    let m = fresh () |> T.withValidation (fun _ -> T.Invalid "no") |> typeText "ab"
    let buffer = Paint.render 12 1 (T.view false m)
    Assert.Equal(m.ErrorStyle, (Buffer.get buffer 2 0).Style)

// --- suggestions ---

let private withPool pool model = { model with T.Suggestions = pool }

let private pool = [ "Ada Lovelace"; "Alan Turing"; "Grace Hopper" ]

[<Fact>]
let ``matches ignore case and need something typed`` () =
    let m = fresh () |> withPool pool
    Assert.Equal(None, T.currentSuggestion m)
    Assert.Equal(Some "Grace Hopper", T.currentSuggestion (typeText "gr" m))

[<Fact>]
let ``an entry the same length as the value is not a match`` () =
    let m = fresh () |> withPool [ "abc" ] |> typeText "abc"
    Assert.Equal(None, T.currentSuggestion m)

[<Fact>]
let ``suggestions only apply with the cursor at the end`` () =
    let m = fresh () |> withPool pool |> typeText "gr" |> press left
    Assert.Equal(None, T.currentSuggestion m)

[<Fact>]
let ``tab accepts the suggestion`` () =
    let m = fresh () |> withPool pool |> typeText "gr" |> press (KeyEvent.plain Key.Tab)
    Assert.Equal("Grace Hopper", m.Value)
    Assert.Equal(12, m.Pos)

[<Fact>]
let ``tab with no suggestion changes nothing`` () =
    let m = fresh () |> withPool pool |> typeText "zz" |> press (KeyEvent.plain Key.Tab)
    Assert.Equal("zz", m.Value)

[<Fact>]
let ``accepting is one undo step`` () =
    let m = fresh () |> withPool pool |> typeText "gr" |> press (KeyEvent.plain Key.Tab)
    Assert.Equal("gr", (press undo m).Value)

[<Fact>]
let ``ctrl+n and ctrl+p cycle through the matches`` () =
    let m = fresh () |> withPool pool |> typeText "a"
    Assert.Equal(Some "Ada Lovelace", T.currentSuggestion m)
    let next = press (KeyEvent.ctrl (Key.Char 'n')) m
    Assert.Equal(Some "Alan Turing", T.currentSuggestion next)
    Assert.Equal(Some "Ada Lovelace", T.currentSuggestion (press (KeyEvent.ctrl (Key.Char 'n')) next))
    Assert.Equal(Some "Alan Turing", T.currentSuggestion (press (KeyEvent.ctrl (Key.Char 'p')) m))

[<Fact>]
let ``a change to the value goes back to the first match`` () =
    // "a" matches all three; Ctrl+N moves to "Adam Smith". "ad" still matches two
    // ("Ada Lovelace", "Adam Smith"), so the reset to index 0 is what picks Ada, not luck.
    let m =
        fresh ()
        |> withPool [ "Ada Lovelace"; "Adam Smith"; "Alan Turing" ]
        |> typeText "a"
        |> press (KeyEvent.ctrl (Key.Char 'n'))
        |> typeText "d"

    Assert.Equal(0, m.SuggestionIndex)
    Assert.Equal(Some "Ada Lovelace", T.currentSuggestion m)

[<Fact>]
let ``a pool changed under the index does not throw`` () =
    let m =
        fresh ()
        |> withPool pool
        |> typeText "a"
        |> press (KeyEvent.ctrl (Key.Char 'n'))
        |> withPool [ "Ada Lovelace" ]

    Assert.Equal(Some "Ada Lovelace", T.currentSuggestion m)

[<Fact>]
let ``a negative suggestion index does not throw`` () =
    let base' = fresh () |> withPool pool |> typeText "a"
    let m = { base' with T.SuggestionIndex = -1 }

    Assert.Equal(Some "Alan Turing", T.currentSuggestion m)

[<Fact>]
let ``accepting a suggestion respects the character limit`` () =
    let m =
        { (fresh ()) with CharLimit = 5 }
        |> withPool [ "Grace Hopper"; "Greg" ]
        |> typeText "gr"

    Assert.Equal(Some "Greg", T.currentSuggestion m)
    Assert.Equal("Greg", (press (KeyEvent.plain Key.Tab) m).Value)

[<Fact>]
let ``a suggestion with control characters is cleaned before it is offered`` () =
    let m = fresh () |> withPool [ "ab\tcd" ] |> typeText "a"

    Assert.Equal(Some "abcd", T.currentSuggestion m)
    Assert.Equal("abcd", (press (KeyEvent.plain Key.Tab) m).Value)

[<Fact>]
let ``a suggestion entry ending in a lone high surrogate is offered and accepted without it`` () =
    let m = fresh () |> withPool [ "ab" + string '\uD83D' ] |> typeText "a"

    Assert.Equal(Some "ab", T.currentSuggestion m)
    Assert.Equal("ab", (press (KeyEvent.plain Key.Tab) m).Value)

[<Fact>]
let ``ghost text shows after the value only when focused`` () =
    let m = fresh () |> withPool pool |> typeText "gr"
    Assert.Equal("> grace Hopper", line 20 true m)
    Assert.Equal("> gr", line 20 false m)
    let buffer = Paint.render 20 1 (T.view true m)
    Assert.Equal(m.SuggestionStyle, (Buffer.get buffer 6 0).Style)

[<Fact>]
let ``ghost text is cut to the width`` () =
    let m = { (fresh ()) with Width = 6 } |> withPool pool |> typeText "gr"
    Assert.Equal("> grace", line 20 true m)

[<Fact>]
let ``a password never shows a suggestion`` () =
    let m =
        { (fresh ()) with
            Echo = T.Password '*' }
        |> withPool [ "secret-password" ]
        |> typeText "se"

    Assert.Equal("> **", line 30 true m)
    Assert.Equal(None, T.currentSuggestion m)
    Assert.Equal("se", (press (KeyEvent.plain Key.Tab) m).Value)

[<Fact>]
let ``a hidden field never shows a suggestion`` () =
    let m =
        { (fresh ()) with Echo = T.Hidden }
        |> withPool [ "secret-password" ]
        |> typeText "se"

    Assert.Equal(None, T.currentSuggestion m)
    Assert.Equal(">", line 30 true m)
    Assert.Equal("se", (press (KeyEvent.plain Key.Tab) m).Value)
