namespace Fansi

open System
open Elmish
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module ViewportComponent =

    type Message =
        | LineUp
        | LineDown
        | PageUp
        | PageDown
        | HalfPageUp
        | HalfPageDown
        | Top
        | Bottom
        | KeyInput of KeyEvent
        | MouseInput of MouseEvent

    type Model =
        {
            /// One node per paragraph: Ui.text or Ui.line.
            Content: Node list
            /// Cells. Lines wrap to this; 0 or below means no wrapping.
            Width: int
            /// Rows shown.
            Height: int
            /// The first wrapped row shown.
            YOffset: int
            /// Rows per mouse-wheel notch.
            WheelStep: int
        }

    let init width height content =
        { Content = content
          Width = width
          Height = height
          YOffset = 0
          WheelStep = 3 },
        Cmd.none

    // Runs.wrap gives no rows at all for a width of 0, where the Viewport means
    // "no wrapping".
    let private wrapWidth model =
        if model.Width <= 0 then Int32.MaxValue else model.Width

    let private isPlainAscii (text: string) =
        text |> Seq.forall (fun c -> c >= ' ' && c <= '~')

    /// Rows Runs.wrap gives one paragraph, counted without building them. It walks
    /// the glyphs the way wrap does, so the two always agree.
    let internal rowsIn width (node: Node) =
        match node with
        // Every plain ASCII character is one cell, so a log line needs no glyph walk.
        | Text(t, _, _) when not (isNull t) && isPlainAscii t -> 1 + max 0 (t.Length - 1) / width
        | _ ->
            let mutable rows = 1
            let mutable used = 0

            for run in Runs.ofNode Style.Default node do
                let segments = run.Text.Split '\n'

                for i in 0 .. segments.Length - 1 do
                    if i > 0 then
                        rows <- rows + 1
                        used <- 0

                    for g in Width.glyphs segments[i] do
                        if g.Cells <= width then
                            if used + g.Cells > width then
                                rows <- rows + 1
                                used <- 0

                            used <- used + g.Cells

            rows

    // Counted on every call rather than cached, so a model changed by hand
    // ({ m with Width = 10 }) never holds rows that no longer match.
    let private paragraphRows model =
        model.Content |> List.map (rowsIn (wrapWidth model))

    // A view with no height shows nothing, so it has nowhere to scroll.
    let private lastOffset height count =
        if height <= 0 then 0 else max 0 (count - height)

    let private clamp height count offset =
        offset |> min (lastOffset height count) |> max 0

    let private scrollBy delta model =
        let count = List.sum (paragraphRows model)
        let from = clamp model.Height count model.YOffset

        { model with
            YOffset = clamp model.Height count (from + delta) }

    let private halfPage model = max 1 (model.Height / 2)

    let rec update msg model =
        match msg with
        | LineUp -> scrollBy -1 model, Cmd.none
        | LineDown -> scrollBy 1 model, Cmd.none
        | PageUp -> scrollBy -model.Height model, Cmd.none
        | PageDown -> scrollBy model.Height model, Cmd.none
        | HalfPageUp -> scrollBy -(halfPage model) model, Cmd.none
        | HalfPageDown -> scrollBy (halfPage model) model, Cmd.none
        | Top -> { model with YOffset = 0 }, Cmd.none
        | Bottom ->
            { model with
                YOffset = lastOffset model.Height (List.sum (paragraphRows model)) },
            Cmd.none
        | KeyInput key ->
            match key.Key with
            | Key.Up -> update LineUp model
            | Key.Down -> update LineDown model
            | Key.PageUp -> update PageUp model
            | Key.PageDown
            | Key.Char ' ' -> update PageDown model
            | Key.Char 'u' when key.Ctrl -> update HalfPageUp model
            | Key.Char 'd' when key.Ctrl -> update HalfPageDown model
            | Key.Home -> update Top model
            | Key.End -> update Bottom model
            | _ -> model, Cmd.none
        | MouseInput mouse ->
            match mouse.Button with
            | MouseButton.ScrollUp -> scrollBy -model.WheelStep model, Cmd.none
            | MouseButton.ScrollDown -> scrollBy model.WheelStep model, Cmd.none
            | _ -> model, Cmd.none

    let atTop model =
        clamp model.Height (List.sum (paragraphRows model)) model.YOffset = 0

    let atBottom model =
        let count = List.sum (paragraphRows model)
        clamp model.Height count model.YOffset = lastOffset model.Height count

    let scrollPercent model =
        let count = List.sum (paragraphRows model)
        let last = lastOffset model.Height count

        if last = 0 then
            100
        else
            clamp model.Height count model.YOffset * 100 / last

    /// Replaces the content. A view at the bottom stays at the bottom, so a log
    /// follows new lines; a view scrolled up keeps its place.
    let setContent content (model: Model) =
        let follow = atBottom model
        let next = { model with Content = content }
        let count = List.sum (paragraphRows next)

        { next with
            YOffset =
                if follow then
                    lastOffset next.Height count
                else
                    clamp next.Height count model.YOffset }

    /// One paragraph per line. A single trailing newline ends the last line, so
    /// a file's final newline does not add a blank paragraph.
    let setText (text: string) model =
        let text =
            if text.EndsWith '\n' then
                text[.. text.Length - 2]
            else
                text

        setContent (text.Split '\n' |> Array.map Ui.text |> List.ofArray) model

    /// Changes the size. Rewrapping moves every row, so the view keeps the
    /// paragraph that held its top row, rather than a row number that now points
    /// somewhere unrelated.
    let setSize width height (model: Model) =
        let resized =
            { model with
                Width = width
                Height = height }

        // Runs.wrap gives every paragraph at least one row, so the starts rise.
        let starts m = paragraphRows m |> List.scan (+) 0

        let oldStarts = starts model
        let newStarts = starts resized
        let count = List.last newStarts
        let oldCount = List.last oldStarts
        let top = clamp model.Height oldCount model.YOffset

        if top = lastOffset model.Height oldCount then
            { resized with
                YOffset = lastOffset height count }
        else
            // Not at the bottom means there is content and the top row is inside it.
            let index = oldStarts |> List.findIndexBack (fun start -> start <= top)

            { resized with
                YOffset = clamp height count (List.item index newStarts) }

    let view model : Node =
        let height = max 0 model.Height
        let counts = paragraphRows model
        let count = List.sum counts
        let first = clamp model.Height count model.YOffset

        // Only the paragraphs that hold the shown rows are wrapped.
        let skipped =
            counts
            |> List.scan (+) 0
            |> List.takeWhile (fun start -> start <= first)
            |> List.length

        let paragraph = max 0 (skipped - 1)
        let start = counts |> List.truncate paragraph |> List.sum

        let shown =
            model.Content
            |> List.skip paragraph
            |> Seq.collect (fun node -> Runs.wrap (Runs.ofNode Style.Default node) (wrapWidth model))
            |> Seq.skip (first - start)
            |> Seq.truncate height
            |> List.ofSeq

        // Runs.ofNode has already cascaded each run's style, so a run draws as it stands.
        let line (row: Run list) =
            Ui.line (row |> List.map (fun run -> Ui.text run.Text |> Ui.style run.Style))

        Ui.col (List.map line shown @ List.replicate (height - shown.Length) (Ui.text ""))
