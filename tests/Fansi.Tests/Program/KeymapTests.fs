module Fansi.Tests.KeymapTests

open Xunit
open Fansi
open Fansi.Core

[<Fact>]
let ``a binding matches its key with no modifiers`` () =
    let bind = Keymap.KeyBind.create [ KeyEvent.plain Key.Left ]
    Assert.True(Keymap.``match`` bind (KeyEvent.plain Key.Left))
    Assert.False(Keymap.``match`` bind (KeyEvent.plain Key.Right))

[<Fact>]
let ``a binding with no modifiers does not match one that has them`` () =
    let bind = Keymap.KeyBind.create [ KeyEvent.plain Key.Left ]

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
            [ KeyEvent.plain Key.Backspace
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
    let bind = Keymap.KeyBind.create [ KeyEvent.plain Key.Enter ]
    Assert.False(Keymap.``match`` (Keymap.toggleEnable bind) (KeyEvent.plain Key.Enter))
    Assert.False(Keymap.``match`` (Keymap.unbind bind) (KeyEvent.plain Key.Enter))

[<Fact>]
let ``a character binding matches that character only`` () =
    let bind = Keymap.KeyBind.create [ KeyEvent.plain (Key.Char 'q') ]
    Assert.True(Keymap.``match`` bind (KeyEvent.plain (Key.Char 'q')))
    Assert.False(Keymap.``match`` bind (KeyEvent.plain (Key.Char 'w')))

[<Fact>]
let ``the ctrl and alt helpers set one modifier each`` () =
    Assert.Equal(
        { Key = Key.Char 'a'
          Ctrl = true
          Alt = false
          Shift = false },
        KeyEvent.ctrl (Key.Char 'a')
    )

    Assert.Equal(
        { Key = Key.Char 'f'
          Ctrl = false
          Alt = true
          Shift = false },
        KeyEvent.alt (Key.Char 'f')
    )

[<Fact>]
let ``a ctrl binding matches ctrl only`` () =
    let bind = Keymap.KeyBind.create [ KeyEvent.ctrl (Key.Char 'a') ]
    Assert.True(Keymap.``match`` bind (KeyEvent.ctrl (Key.Char 'a')))
    Assert.False(Keymap.``match`` bind (KeyEvent.plain (Key.Char 'a')))
    Assert.False(Keymap.``match`` bind (KeyEvent.alt (Key.Char 'a')))
