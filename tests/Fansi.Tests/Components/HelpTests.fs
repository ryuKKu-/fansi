module Fansi.Tests.HelpTests

open Xunit
open Fansi
open Fansi.Core

module H = HelpComponent

let private render w h node =
    Paint.render w h node |> Buffer.toLines |> List.map (fun l -> l.TrimEnd())

let private bind key description =
    Keymap.setHelp
        (Keymap.KeyBind.create [ KeyEvent.plain (Key.Char 'x') ])
        (Some
            { Keymap.Key = key
              Keymap.Description = description })

let private up = bind "↑" "up"
let private down = bind "↓" "down"
let private quit = bind "q" "quit"
let private help = bind "?" "help"

let private short width bindings =
    H.view bindings [] { H.init () with Width = width }

let private full width groups =
    H.view
        []
        groups
        { H.init () with
            Width = width
            ShowAll = true }

[<Fact>]
let ``short help joins the entries on one line`` () =
    Assert.Equal<string list>([ "↑ up • ↓ down • q quit" ], render 40 1 (short 0 [ up; down; quit ]))

[<Fact>]
let ``keys and descriptions keep their own styles`` () =
    let buffer = Paint.render 40 1 (short 0 [ up ])
    Assert.True((Buffer.get buffer 0 0).Style.Bold)
    Assert.False((Buffer.get buffer 2 0).Style.Bold)
    Assert.Equal(Color.BrightBlack, (Buffer.get buffer 2 0).Style.FgColor)

[<Fact>]
let ``entries that do not fit are dropped whole`` () =
    Assert.Equal<string list>([ "↑ up …" ], render 40 1 (short 12 [ up; down; quit ]))
    Assert.Equal<string list>([ "↑ up" ], render 40 1 (short 5 [ up; down; quit ]))

[<Fact>]
let ``a wide description is counted in cells`` () =
    let wide = bind "w" "日本"
    // "w 日本" is 6 cells, so with " • " the second entry needs 13.
    Assert.Equal<string list>([ "↑ up …" ], render 40 1 (short 12 [ up; wide ]))
    Assert.Equal<string list>([ "↑ up • w 日本" ], render 40 1 (short 13 [ up; wide ]))

[<Fact>]
let ``bindings that cannot be used or have no help are left out`` () =
    let disabled = Keymap.toggleEnable down
    let noKeys = Keymap.setKeys (bind "x" "gone") []
    let noHelp = Keymap.KeyBind.create [ KeyEvent.plain Key.Enter ]
    Assert.Equal<string list>([ "↑ up • q quit" ], render 40 1 (short 0 [ up; disabled; noKeys; noHelp; quit ]))

[<Fact>]
let ``nothing to show draws an empty line`` () =
    Assert.Equal<string list>([ "" ], render 10 1 (short 0 []))
    Assert.Equal<string list>([ "" ], render 10 1 (short 2 [ up ]))

[<Fact>]
let ``full help shows one column per group`` () =
    Assert.Equal<string list>(
        [ "↑  up     q  quit"; "↓  down   ?  help" ],
        render 40 2 (full 0 [ [ up; down ]; [ quit; help ] ])
    )

[<Fact>]
let ``keys are padded within their column`` () =
    let ctrlC = bind "ctrl+c" "quit"
    let back = bind "q" "back"
    Assert.Equal<string list>([ "ctrl+c  quit"; "q       back" ], render 40 2 (full 0 [ [ ctrlC; back ] ]))

[<Fact>]
let ``an empty group is left out`` () =
    Assert.Equal<string list>([ "q  quit" ], render 40 1 (full 0 [ []; [ quit ] ]))

[<Fact>]
let ``columns that do not fit are left out`` () =
    Assert.Equal<string list>([ "↑  up   …"; "↓  down" ], render 40 2 (full 12 [ [ up; down ]; [ quit; help ] ]))

[<Fact>]
let ``toggle switches between short and full help`` () =
    let m = H.init ()
    Assert.False(m.ShowAll)
    Assert.True((H.toggle m).ShowAll)
    Assert.False((H.toggle (H.toggle m)).ShowAll)
