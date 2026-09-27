module Fansi.Tests.CursorTests

open Xunit
open Fansi
open Fansi.Core

let private cell node = Buffer.get (Paint.render 1 1 node) 0 0

[<Fact>]
let ``each cursor gets its own id`` () =
    Assert.NotEqual(Cursor.create().Id, Cursor.create().Id)

[<Fact>]
let ``a blink tick for this cursor toggles it`` () =
    let c = Cursor.create ()
    let ticked, _ = Cursor.update (Cursor.BlinkTick c.Id) c
    Assert.Equal(not c.Blink, ticked.Blink)

[<Fact>]
let ``a blink tick for another cursor changes nothing`` () =
    let c = Cursor.create ()
    let other = Cursor.create ()
    let ticked, _ = Cursor.update (Cursor.BlinkTick other.Id) c
    Assert.Equal(c.Blink, ticked.Blink)

[<Fact>]
let ``a static cursor does not blink`` () =
    let c =
        { Cursor.create () with
            Type = Cursor.Static }

    let ticked, _ = Cursor.update (Cursor.BlinkTick c.Id) c
    Assert.Equal(c.Blink, ticked.Blink)

[<Fact>]
let ``only a focused blinking cursor subscribes`` () =
    let c = Cursor.create ()
    Assert.Equal<string list list>([ [ "fansi"; "cursor"; string c.Id ] ], Cursor.subscribe true c |> List.map fst)
    Assert.Empty(Cursor.subscribe false c)
    Assert.Empty(Cursor.subscribe true { c with Type = Cursor.Static })

[<Fact>]
let ``two cursors subscribe under different ids`` () =
    let a = Cursor.subscribe true (Cursor.create ()) |> List.map fst
    let b = Cursor.subscribe true (Cursor.create ()) |> List.map fst
    Assert.NotEqual<string list list>(a, b)

[<Fact>]
let ``a focused cursor in its visible half is highlighted`` () =
    let c = { Cursor.create () with Blink = true }
    let shown = cell (Cursor.view true "x" Style.Default c)
    Assert.Equal('x', shown.Char)
    Assert.Equal(Color.Cyan, shown.Style.BgColor)
    Assert.Equal(Color.Black, shown.Style.FgColor)

[<Fact>]
let ``an unfocused or blinked-off cursor keeps the text's own style`` () =
    let style =
        { Style.Default with
            FgColor = Color.Red }

    let c = { Cursor.create () with Blink = true }
    Assert.Equal(style, (cell (Cursor.view false "x" style c)).Style)
    Assert.Equal(style, (cell (Cursor.view true "x" style { c with Blink = false })).Style)

[<Fact>]
let ``a static cursor always shows and a hidden one never does`` () =
    let c = { Cursor.create () with Blink = false }
    Assert.Equal(Color.Cyan, (cell (Cursor.view true "x" Style.Default { c with Type = Cursor.Static })).Style.BgColor)

    Assert.Equal(
        Color.Default,
        (cell (
            Cursor.view
                true
                "x"
                Style.Default
                { c with
                    Type = Cursor.Hidden
                    Blink = true }
        ))
            .Style.BgColor
    )
