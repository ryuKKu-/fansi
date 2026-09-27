module Fansi.Tests.KeymapTests

open Xunit
open Fansi
open Fansi.Core

[<Fact>]
let ``a binding matches its key with no modifiers`` () =
    let bind = Keymap.KeyBind.create [ Keymap.KeyControl.plain Key.Left ]
    Assert.True(Keymap.``match`` bind (KeyEvent.plain Key.Left))
    Assert.False(Keymap.``match`` bind (KeyEvent.plain Key.Right))

[<Fact>]
let ``a binding with no modifiers does not match one that has them`` () =
    let bind = Keymap.KeyBind.create [ Keymap.KeyControl.plain Key.Left ]

    Assert.False(
        Keymap.``match``
            bind
            { Key = Key.Left
              Ctrl = true
              Alt = false
              Shift = false }
    )

[<Fact>]
let ``a binding matches any of its keys`` () =
    let bind =
        Keymap.KeyBind.create
            [ Keymap.KeyControl.plain Key.Backspace
              { Key = Key.Backspace
                Ctrl = true
                Alt = false
                Shift = false } ]

    Assert.True(Keymap.``match`` bind (KeyEvent.plain Key.Backspace))

    Assert.True(
        Keymap.``match``
            bind
            { Key = Key.Backspace
              Ctrl = true
              Alt = false
              Shift = false }
    )

[<Fact>]
let ``a disabled or unbound binding matches nothing`` () =
    let bind = Keymap.KeyBind.create [ Keymap.KeyControl.plain Key.Enter ]
    Assert.False(Keymap.``match`` (Keymap.toggleEnable bind) (KeyEvent.plain Key.Enter))
    Assert.False(Keymap.``match`` (Keymap.unbind bind) (KeyEvent.plain Key.Enter))

[<Fact>]
let ``a character binding matches that character only`` () =
    let bind = Keymap.KeyBind.create [ Keymap.KeyControl.plain (Key.Char 'q') ]
    Assert.True(Keymap.``match`` bind (KeyEvent.plain (Key.Char 'q')))
    Assert.False(Keymap.``match`` bind (KeyEvent.plain (Key.Char 'w')))
