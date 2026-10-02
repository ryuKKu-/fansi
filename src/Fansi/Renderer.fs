namespace Fansi

open System
open System.Text
open Fansi.Core

[<Measure>]
type FPS

module AnsiSequence =
    /// Erases what is visible and homes the cursor. It leaves out ESC[3J, which
    /// wipes scrollback: some terminals apply it to the main screen's history even
    /// from the alternate screen.
    [<Literal>]
    let eraseVisibleScreen = "\x1b[2J\x1b[1;1H"

    [<Literal>]
    let enableAltScreenBuffer = "\x1b[?1049h"

    [<Literal>]
    let disableAltScreenBuffer = "\x1b[?1049l"

    [<Literal>]
    let hideCursor = "\x1b[?25l"

    [<Literal>]
    let showCursor = "\x1b[?25h"

    [<Literal>]
    let enableMouseTracking = "\x1b[?1006h\x1b[?1003h"

    [<Literal>]
    let disableMouseTracking = "\x1b[?1003l\x1b[?1006l"

    [<Literal>]
    let enableBracketedPaste = "\x1b[?2004h"

    [<Literal>]
    let disableBracketedPaste = "\x1b[?2004l"

    [<Literal>]
    let enableFocusReporting = "\x1b[?1004h"

    [<Literal>]
    let disableFocusReporting = "\x1b[?1004l"

    [<Literal>]
    let resetStyle = "\x1b[0m"

    let moveCursorTo (x: int) (y: int) = $"\x1b[{y + 1};{x + 1}H"

    let fgColor (color: Color) =
        match color with
        | Color.Default -> ""
        | Color.Black -> "\x1b[30m"
        | Color.Red -> "\x1b[31m"
        | Color.Green -> "\x1b[32m"
        | Color.Yellow -> "\x1b[33m"
        | Color.Blue -> "\x1b[34m"
        | Color.Magenta -> "\x1b[35m"
        | Color.Cyan -> "\x1b[36m"
        | Color.White -> "\x1b[37m"
        | Color.BrightBlack -> "\x1b[90m"
        | Color.BrightRed -> "\x1b[91m"
        | Color.BrightGreen -> "\x1b[92m"
        | Color.BrightYellow -> "\x1b[93m"
        | Color.BrightBlue -> "\x1b[94m"
        | Color.BrightMagenta -> "\x1b[95m"
        | Color.BrightCyan -> "\x1b[96m"
        | Color.BrightWhite -> "\x1b[97m"
        | Color.Rgb(r, g, b) -> $"\x1b[38;2;{r};{g};{b}m"

    let bgColor (color: Color) =
        match color with
        | Color.Default -> ""
        | Color.Black -> "\x1b[40m"
        | Color.Red -> "\x1b[41m"
        | Color.Green -> "\x1b[42m"
        | Color.Yellow -> "\x1b[43m"
        | Color.Blue -> "\x1b[44m"
        | Color.Magenta -> "\x1b[45m"
        | Color.Cyan -> "\x1b[46m"
        | Color.White -> "\x1b[47m"
        | Color.BrightBlack -> "\x1b[100m"
        | Color.BrightRed -> "\x1b[101m"
        | Color.BrightGreen -> "\x1b[102m"
        | Color.BrightYellow -> "\x1b[103m"
        | Color.BrightBlue -> "\x1b[104m"
        | Color.BrightMagenta -> "\x1b[105m"
        | Color.BrightCyan -> "\x1b[106m"
        | Color.BrightWhite -> "\x1b[107m"
        | Color.Rgb(r, g, b) -> $"\x1b[48;2;{r};{g};{b}m"

    let applyStyle (style: Style) =
        let sb = StringBuilder()
        sb.Append(resetStyle) |> ignore

        if style.Bold then
            sb.Append("\x1b[1m") |> ignore

        if style.Italic then
            sb.Append("\x1b[3m") |> ignore

        if style.Underline then
            sb.Append("\x1b[4m") |> ignore

        if style.Strikethrough then
            sb.Append("\x1b[9m") |> ignore

        sb.Append(fgColor style.FgColor) |> ignore
        sb.Append(bgColor style.BgColor) |> ignore
        sb.ToString()

module Renderer =
    open System.Timers

    type Renderer(fps: int<FPS>, out: IO.TextWriter) =
        [<Literal>]
        let defaultFPS = 60

        let fpsToRender =
            let fps = int fps
            if fps < 1 then defaultFPS else fps

        let frameRate = TimeSpan.FromSeconds(1.0 / float fpsToRender)
        let ticker = new Timer(frameRate)

        let mutable currentBuffer = Buffer.create 0 0
        let mutable previousBuffer = Buffer.create 0 0
        let mutable dirty = false
        // Set when the size changes after something is on screen. The new frame is
        // diffed against a blank buffer, so any cell it leaves blank would keep
        // whatever the old frame had there.
        let mutable eraseFirst = false
        let mutable painted = false
        // A timer tick can still arrive after Stop. Past this point nothing may
        // paint, because the caller is about to leave the alternate screen.
        let mutable stopped = false

        new(fps: int<FPS>) = Renderer(fps, Console.Out)

        member this.Flush() =
            lock this (fun () ->
                if stopped || not dirty then
                    ()
                else
                    dirty <- false
                    let w = currentBuffer.Width
                    let h = currentBuffer.Height
                    let sb = StringBuilder()
                    let mutable lastStyle = Style.Default
                    let mutable cursorX = -1
                    let mutable cursorY = -1

                    // In the same write as the redraw, so the screen never shows
                    // blank between the two.
                    if eraseFirst then
                        eraseFirst <- false
                        sb.Append(AnsiSequence.eraseVisibleScreen) |> ignore
                        cursorX <- 0
                        cursorY <- 0

                    for y in 0 .. h - 1 do
                        let mutable x = 0

                        while x < w do
                            let cur = Buffer.get currentBuffer x y
                            let prev = Buffer.get previousBuffer x y

                            if cur <> prev then
                                if cursorX <> x || cursorY <> y then
                                    sb.Append(AnsiSequence.moveCursorTo x y) |> ignore
                                    cursorX <- x
                                    cursorY <- y

                                let mutable runEnd = x

                                while runEnd < w
                                      && Buffer.get currentBuffer runEnd y <> Buffer.get previousBuffer runEnd y do
                                    runEnd <- runEnd + 1

                                for rx in x .. runEnd - 1 do
                                    let cell = Buffer.get currentBuffer rx y

                                    if cell.Style <> lastStyle then
                                        sb.Append(AnsiSequence.applyStyle cell.Style) |> ignore
                                        lastStyle <- cell.Style

                                    sb.Append(cell.Char) |> ignore

                                cursorX <- runEnd
                                x <- runEnd
                            else
                                x <- x + 1

                    if sb.Length > 0 then
                        sb.Append(AnsiSequence.resetStyle) |> ignore
                        out.Write(sb.ToString())
                        out.Flush()

                    if w > 0 && h > 0 then
                        painted <- true

                    previousBuffer <- currentBuffer)

        member this.Start() =
            if not ticker.Enabled then
                ticker.Elapsed.Add(fun _ -> this.Flush())
                ticker.Start()

        /// Hand a frame to the renderer. The renderer takes ownership of the buffer:
        /// do not mutate or reuse it afterwards, because it becomes the next frame's
        /// diff baseline and later writes to it would make those cells look unchanged.
        /// Give every frame a fresh buffer - Paint.render returns one.
        member this.SetFrame(buffer: Buffer) =
            lock this (fun () ->
                if buffer.Width <> previousBuffer.Width || buffer.Height <> previousBuffer.Height then
                    previousBuffer <- Buffer.create buffer.Width buffer.Height
                    eraseFirst <- eraseFirst || painted

                currentBuffer <- buffer
                dirty <- true)

        /// Paints what is still pending, then stops for good.
        member this.Stop() =
            if ticker.Enabled then
                ticker.Stop()

                lock this (fun () ->
                    this.Flush()
                    stopped <- true)

        member this.Execute(str: string) =
            lock this (fun () ->
                out.Write str
                out.Flush())
