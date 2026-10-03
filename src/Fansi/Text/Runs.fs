namespace Fansi.Core

open System.Text

module Runs =
    /// Combine a node's own style with the one it inherits from its parent.
    /// A foreground only carries down when the child leaves it Default. The four
    /// attributes are OR-ed, so a child cannot switch off bold inside a bold parent;
    /// that is a limitation we accept, since Style has no "off" value to say it with.
    /// Background is absent on purpose: Buffer.writeText already keeps a parent's
    /// background visible under its children.
    let cascade (parent: Style) (own: Style) =
        { own with
            FgColor =
                if own.FgColor = Color.Default then
                    parent.FgColor
                else
                    own.FgColor
            Bold = own.Bold || parent.Bold
            Italic = own.Italic || parent.Italic
            Underline = own.Underline || parent.Underline
            Strikethrough = own.Strikethrough || parent.Strikethrough }

    /// The styled runs of a node, in reading order. A container or line inside is
    /// flattened: only its text and style count, never its props.
    let rec ofNode (parent: Style) (node: Node) : Run list =
        // Inside a line, a nested node's background carries down as well, because
        // only its text is drawn. Its own box never is.
        let carry (own: Style) =
            { cascade parent own with
                BgColor =
                    if own.BgColor = Color.Default then
                        parent.BgColor
                    else
                        own.BgColor }

        match node with
        | Text(t, s, _) -> [ { Text = t; Style = carry s } ]
        | Container(kids, s, _)
        | Line(kids, s, _) -> kids |> List.collect (ofNode (carry s))

    let width (row: Run list) =
        row |> List.sumBy (fun r -> Width.ofString r.Text)

    /// Break runs into rows no wider than maxWidth cells. A newline starts a row.
    /// Text is cut hard, with no word awareness, but never inside a character: one
    /// that does not fit what is left of a row moves to the next, and one wider than
    /// a whole row fits nowhere and is dropped.
    let wrap (runs: Run list) (maxWidth: int) : Run list list =
        if maxWidth <= 0 then
            []
        else
            let rows = ResizeArray<Run list>()
            let row = ResizeArray<Run>()
            let text = StringBuilder()
            let style = ref Style.Default
            let used = ref 0

            let flush () =
                if text.Length > 0 then
                    row.Add
                        { Text = text.ToString()
                          Style = style.Value }

                    text.Clear() |> ignore

            let endRow () =
                flush ()
                rows.Add(List.ofSeq row)
                row.Clear()
                used.Value <- 0

            for run in runs do
                flush ()
                style.Value <- run.Style
                let segments = run.Text.Split('\n')

                for i in 0 .. segments.Length - 1 do
                    if i > 0 then
                        endRow ()

                    for g in Width.glyphs segments[i] do
                        if g.Cells <= maxWidth then
                            if used.Value + g.Cells > maxWidth then
                                endRow ()

                            text.Append(g.Chars) |> ignore
                            used.Value <- used.Value + g.Cells

            endRow ()
            List.ofSeq rows

    /// The runs cut to at most maxWidth cells. It stops at the first character
    /// that would cross the edge, so a wide character is dropped rather than halved.
    let truncate (maxWidth: int) (runs: Run list) : Run list =
        let kept = ResizeArray<Run>()
        let mutable used = 0
        let mutable full = false

        for run in runs do
            if not full then
                let text = StringBuilder()

                for g in Width.glyphs run.Text do
                    if not full then
                        if used + g.Cells > maxWidth then
                            full <- true
                        else
                            text.Append(g.Chars) |> ignore
                            used <- used + g.Cells

                if text.Length > 0 then
                    kept.Add { run with Text = text.ToString() }

        List.ofSeq kept
