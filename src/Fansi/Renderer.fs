namespace Fansi

open System
open System.Text
open Fansi.Core
open Fansi.Core.Layout

[<Measure>] type FPS

module AnsiSequence =
    let [<Literal>] clearScreen = "\x1b[2J\x1b[3J\x1b[1;1H"
    let [<Literal>] enableAltScreenBuffer = "\x1b[?1049h"
    let [<Literal>] disableAltScreenBuffer = "\x1b[?1049l"
    let [<Literal>] moveCursorToOrigin = "\x1b[1;1H"
    let [<Literal>] hideCursor = "\x1b[?25l"
    let [<Literal>] showCursor = "\x1b[?25h"
    let [<Literal>] enableMouseTracking = "\x1b[?1006h\x1b[?1003h"
    let [<Literal>] disableMouseTracking = "\x1b[?1003l\x1b[?1006l"
    let [<Literal>] resetStyle = "\x1b[0m"

    let moveCursorTo (x: int) (y: int) = $"\x1b[{y + 1};{x + 1}H"

    let fgColor (color: Color.Color) =
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

    let bgColor (color: Color.Color) =
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
        if style.Bold then sb.Append("\x1b[1m") |> ignore
        if style.Italic then sb.Append("\x1b[3m") |> ignore
        if style.Underline then sb.Append("\x1b[4m") |> ignore
        if style.Strikethrough then sb.Append("\x1b[9m") |> ignore
        sb.Append(fgColor style.FgColor) |> ignore
        sb.Append(bgColor style.BgColor) |> ignore
        sb.ToString()

module Renderer =
    open System.Timers

    type Cell =
        { Char: char
          Style: Style }

        static member Empty = { Char = ' '; Style = Style.Default }

    type Renderer(fps: int<FPS>) =
        let [<Literal>] defaultFPS = 60
        let fpsToRender =
            let fps = int fps
            if fps < 1 then defaultFPS else fps

        let frameRate = TimeSpan.FromSeconds(1.0 / float fpsToRender)
        let ticker = new Timer(frameRate)

        let mutable currentBuffer: Cell[,] = Array2D.init 0 0 (fun _ _ -> Cell.Empty)
        let mutable previousBuffer: Cell[,] = Array2D.init 0 0 (fun _ _ -> Cell.Empty)
        let mutable pendingSpans: Span list = []
        let mutable pendingBorderSpans: Span list = []
        let mutable bufferWidth = 0
        let mutable bufferHeight = 0
        let mutable dirty = false

        let ensureBufferSize (w: int) (h: int) =
            if w <> bufferWidth || h <> bufferHeight then
                bufferWidth <- w
                bufferHeight <- h
                currentBuffer <- Array2D.init h w (fun _ _ -> Cell.Empty)
                previousBuffer <- Array2D.init h w (fun _ _ -> Cell.Empty)

        let clearBuffer (buf: Cell[,]) =
            for y in 0 .. Array2D.length1 buf - 1 do
                for x in 0 .. Array2D.length2 buf - 1 do
                    buf[y, x] <- Cell.Empty

        let writeSpanToBuffer (buf: Cell[,]) (span: Span) =
            let h = Array2D.length1 buf
            let w = Array2D.length2 buf
            if span.Y >= 0 && span.Y < h then
                let mutable col = span.X
                for c in span.Text do
                    if col >= 0 && col < w then
                        buf[span.Y, col] <- { Char = c; Style = span.Style }
                    col <- col + 1

        member this.Flush() =
            lock this (fun () ->
                if not dirty then ()
                else
                    dirty <- false
                    let w = bufferWidth
                    let h = bufferHeight
                    if w = 0 || h = 0 then ()
                    else
                        clearBuffer currentBuffer

                        for span in pendingBorderSpans do
                            writeSpanToBuffer currentBuffer span
                        for span in pendingSpans do
                            writeSpanToBuffer currentBuffer span

                        let sb = StringBuilder()
                        let mutable lastStyle = Style.Default
                        let mutable cursorX = -1
                        let mutable cursorY = -1

                        for y in 0 .. h - 1 do
                            let mutable x = 0
                            while x < w do
                                let cur = currentBuffer[y, x]
                                let prev = previousBuffer[y, x]
                                if cur <> prev then
                                    if cursorX <> x || cursorY <> y then
                                        sb.Append(AnsiSequence.moveCursorTo x y) |> ignore
                                        cursorX <- x
                                        cursorY <- y

                                    let mutable runEnd = x
                                    while runEnd < w && currentBuffer[y, runEnd] <> previousBuffer[y, runEnd] do
                                        runEnd <- runEnd + 1

                                    for rx in x .. runEnd - 1 do
                                        let cell = currentBuffer[y, rx]
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
                            Console.Out.Write(sb.ToString())
                            Console.Out.Flush()

                        let temp = previousBuffer
                        previousBuffer <- currentBuffer
                        currentBuffer <- temp
            )

        member this.Start() =
            if not ticker.Enabled then
                ticker.Elapsed.Add(fun _ -> this.Flush())
                ticker.Start()

        member this.SetFrame (spans: Span list) (borders: Span list) (width: int) (height: int) =
            lock this (fun () ->
                ensureBufferSize width height
                pendingSpans <- spans
                pendingBorderSpans <- borders
                dirty <- true
            )

        member this.Stop() =
            if ticker.Enabled then
                this.Flush()
                ticker.Stop()

        member this.Execute(str: string) =
            Console.Out.Write str
            Console.Out.Flush()
