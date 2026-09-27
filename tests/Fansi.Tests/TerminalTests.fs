module Fansi.Tests.TerminalTests

open System
open Xunit
open Fansi.Core

[<Fact>]
let ``windows raw input clears the cooked flags and sets vt input`` () =
    let cooked =
        Terminal.Windows.ENABLE_LINE_INPUT
        ||| Terminal.Windows.ENABLE_ECHO_INPUT
        ||| Terminal.Windows.ENABLE_PROCESSED_INPUT

    let raw = Terminal.Windows.rawInputMode cooked

    Assert.Equal(0u, raw &&& Terminal.Windows.ENABLE_LINE_INPUT)
    Assert.Equal(0u, raw &&& Terminal.Windows.ENABLE_ECHO_INPUT)
    Assert.Equal(0u, raw &&& Terminal.Windows.ENABLE_PROCESSED_INPUT)
    Assert.NotEqual(0u, raw &&& Terminal.Windows.ENABLE_VIRTUAL_TERMINAL_INPUT)

[<Fact>]
let ``windows raw input keeps flags it does not own`` () =
    // 0x0008 is ENABLE_WINDOW_INPUT; nothing here should disturb it
    let raw =
        Terminal.Windows.rawInputMode (Terminal.Windows.ENABLE_LINE_INPUT ||| 0x0008u)

    Assert.NotEqual(0u, raw &&& 0x0008u)

[<Fact>]
let ``the windows retry drops vt input and keeps the cooked flags off`` () =
    let wanted =
        Terminal.Windows.rawInputMode (Terminal.Windows.ENABLE_ECHO_INPUT ||| 0x0008u)

    let fallback = Terminal.Windows.withoutVirtualTerminalInput wanted

    Assert.Equal(0u, fallback &&& Terminal.Windows.ENABLE_VIRTUAL_TERMINAL_INPUT)
    Assert.Equal(0u, fallback &&& Terminal.Windows.ENABLE_ECHO_INPUT)
    Assert.NotEqual(0u, fallback &&& 0x0008u)

[<Fact>]
let ``windows vt output only adds its own flag`` () =
    Assert.Equal(0x0005u, Terminal.Windows.vtOutputMode 0x0001u)

[<Fact>]
let ``unix raw flags clear the cooked bits`` () =
    let linux = Terminal.Unix.flagsFor false
    let cookedL = linux.Icanon ||| linux.Echo ||| linux.Isig ||| linux.Iexten
    Assert.Equal(0u, Terminal.Unix.rawLocalFlags linux cookedL)
    Assert.Equal(0u, Terminal.Unix.rawInputFlags linux linux.Ixon)

    let mac = Terminal.Unix.flagsFor true
    let cookedM = mac.Icanon ||| mac.Echo ||| mac.Isig ||| mac.Iexten
    Assert.Equal(0u, Terminal.Unix.rawLocalFlags mac cookedM)
    Assert.Equal(0u, Terminal.Unix.rawInputFlags mac mac.Ixon)

[<Fact>]
let ``unix raw flags keep everything else`` () =
    // TOSTOP, which raw mode has no business touching on either platform
    let linuxTostop = 0x0100u
    let macTostop = 0x00400000u

    let linux = Terminal.Unix.flagsFor false

    Assert.NotEqual(0u, Terminal.Unix.rawLocalFlags linux (linux.Echo ||| linuxTostop) &&& linuxTostop)

    let mac = Terminal.Unix.flagsFor true
    Assert.NotEqual(0u, Terminal.Unix.rawLocalFlags mac (mac.Echo ||| macTostop) &&& macTostop)

[<Fact>]
let ``the two termios layouts differ where they should`` () =
    let linux = Terminal.Unix.layoutFor false
    let mac = Terminal.Unix.layoutFor true

    Assert.Equal(0, linux.IflagOffset)
    Assert.Equal(0, mac.IflagOffset)
    Assert.Equal(12, linux.LflagOffset)
    Assert.Equal(24, mac.LflagOffset)
    Assert.Equal(4, linux.WordSize)
    Assert.Equal(8, mac.WordSize)

[<Fact>]
let ``linux flag words round-trip without disturbing their neighbours`` () =
    let layout = Terminal.Unix.layoutFor false
    let buffer = Array.create Terminal.Unix.BufferSize 0xAAuy

    Terminal.Unix.writeWord buffer layout.IflagOffset layout.WordSize 0x1234UL
    Terminal.Unix.writeWord buffer layout.LflagOffset layout.WordSize 0x5678UL

    Assert.Equal(0x1234UL, Terminal.Unix.readWord buffer layout.IflagOffset layout.WordSize)
    Assert.Equal(0x5678UL, Terminal.Unix.readWord buffer layout.LflagOffset layout.WordSize)
    // the words either side of c_lflag, which nothing here may touch
    Assert.Equal(0xAAAAAAAAUL, Terminal.Unix.readWord buffer 8 layout.WordSize)
    Assert.Equal(0xAAAAAAAAUL, Terminal.Unix.readWord buffer 16 layout.WordSize)

[<Fact>]
let ``macos flag words round-trip without disturbing their neighbours`` () =
    let layout = Terminal.Unix.layoutFor true
    let buffer = Array.create Terminal.Unix.BufferSize 0xAAuy

    Terminal.Unix.writeWord buffer layout.IflagOffset layout.WordSize 0x1234UL
    Terminal.Unix.writeWord buffer layout.LflagOffset layout.WordSize 0x5678UL

    Assert.Equal(0x1234UL, Terminal.Unix.readWord buffer layout.IflagOffset layout.WordSize)
    Assert.Equal(0x5678UL, Terminal.Unix.readWord buffer layout.LflagOffset layout.WordSize)
    // the words either side of c_lflag, which nothing here may touch
    Assert.Equal(0xAAAAAAAAAAAAAAAAUL, Terminal.Unix.readWord buffer 16 layout.WordSize)
    Assert.Equal(0xAAAAAAAAAAAAAAAAUL, Terminal.Unix.readWord buffer 32 layout.WordSize)

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``raw flags land in c_lflag and c_iflag and nowhere else`` (isMacOs: bool) =
    let flags = Terminal.Unix.flagsFor isMacOs
    let layout = Terminal.Unix.layoutFor isMacOs
    let buffer = Array.create Terminal.Unix.BufferSize 0xAAuy
    let cooked = flags.Icanon ||| flags.Echo ||| flags.Isig ||| flags.Iexten
    // a survivor in each field, different from each other, so swapping the two
    // offsets cannot leave both fields looking right
    let tostop = if isMacOs then 0x00400000u else 0x0100u
    let ignbrk = 0x0001u

    Terminal.Unix.writeWord buffer layout.LflagOffset layout.WordSize (uint64 (cooked ||| tostop))
    Terminal.Unix.writeWord buffer layout.IflagOffset layout.WordSize (uint64 (flags.Ixon ||| ignbrk))

    // what the buffer must look like afterwards: cooked bits gone, survivors kept,
    // the rest of the struct as it was
    let expected = Array.copy buffer
    Terminal.Unix.writeWord expected layout.LflagOffset layout.WordSize (uint64 tostop)
    Terminal.Unix.writeWord expected layout.IflagOffset layout.WordSize (uint64 ignbrk)

    Terminal.Unix.applyRawFlags flags layout buffer

    Assert.Equal<byte>(expected, buffer)

[<Fact>]
let ``the upper half of a macos flag word survives`` () =
    let flags = Terminal.Unix.flagsFor true
    let layout = Terminal.Unix.layoutFor true
    let buffer = Array.zeroCreate<byte> Terminal.Unix.BufferSize
    let high = 0x0000000100000000UL

    Terminal.Unix.writeWord buffer layout.LflagOffset layout.WordSize (high ||| uint64 flags.Echo)
    Terminal.Unix.applyRawFlags flags layout buffer

    Assert.Equal(high, Terminal.Unix.readWord buffer layout.LflagOffset layout.WordSize)

[<Fact>]
let ``raw mode is only entered from the foreground process group`` () =
    Assert.True(Terminal.Unix.isForeground 42 42)
    Assert.False(Terminal.Unix.isForeground 42 7)
    // tcgetpgrp answers -1 when stdin is not a terminal
    Assert.False(Terminal.Unix.isForeground -1 -1)

[<Fact>]
let ``entering raw mode without a console changes nothing and still disposes`` () =
    // guarded: an in-process runner that left stdin on the tty would otherwise put
    // the developer's own terminal into raw mode
    if Console.IsInputRedirected then
        use _scope = Terminal.enterRawMode ()
        Assert.False(Terminal.isRaw ())
