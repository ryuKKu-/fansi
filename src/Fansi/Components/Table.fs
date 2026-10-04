namespace Fansi

open System
open Elmish
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module TableComponent =

    type Column = { Title: string; Width: Constraint }

    type Message =
        | Up
        | Down
        | PageUp
        | PageDown
        | Top
        | Bottom
        | KeyInput of KeyEvent
        | MouseInput of MouseEvent

    type Model<'row> =
        {
            Columns: Column list
            Rows: 'row list
            /// One node per column: Ui.text or Ui.line.
            RowToCells: 'row -> Node list
            Cursor: int
            /// The first data row shown.
            Offset: int
            /// Cells. 0 or below means each column takes what it asks for.
            Width: int
            /// Data rows shown. The header adds one more.
            Height: int
            HeaderStyle: Style
            SelectedStyle: Style
            NormalStyle: Style
        }

    let init columns rowToCells width height rows =
        { Columns = columns
          Rows = rows
          RowToCells = rowToCells
          Cursor = 0
          Offset = 0
          Width = width
          Height = height
          HeaderStyle = { Style.Default with Bold = true }
          SelectedStyle =
            { Style.Default with
                FgColor = Color.Black
                BgColor = Color.Cyan }
          NormalStyle = Style.Default },
        Cmd.none

    let private clampCursor (model: Model<'row>) =
        let count = List.length model.Rows

        if count = 0 then
            0
        else
            model.Cursor |> max 0 |> min (count - 1)

    /// Clamps the cursor, then moves the offset only as far as it must to keep
    /// the cursor on screen.
    let private scrolled (model: Model<'row>) =
        let cursor = clampCursor model

        let offset =
            if model.Height <= 0 then
                0
            else
                let last = max 0 (List.length model.Rows - model.Height)

                model.Offset
                |> min cursor
                |> max (cursor - model.Height + 1)
                |> min last
                |> max 0

        { model with
            Cursor = cursor
            Offset = offset }

    let private moveBy (delta: int) (model: Model<'row>) =
        // In int64 so a huge Height used as a page cannot wrap round.
        let target =
            int64 (clampCursor model) + int64 delta |> max 0L |> min (int64 Int32.MaxValue)

        scrolled { model with Cursor = int target }

    let private page (model: Model<'row>) = max 1 model.Height

    let rec update msg (model: Model<'row>) =
        match msg with
        | Up -> moveBy -1 model, Cmd.none
        | Down -> moveBy 1 model, Cmd.none
        | PageUp -> moveBy -(page model) model, Cmd.none
        | PageDown -> moveBy (page model) model, Cmd.none
        | Top -> scrolled { model with Cursor = 0 }, Cmd.none
        | Bottom ->
            scrolled
                { model with
                    Cursor = List.length model.Rows - 1 },
            Cmd.none
        | KeyInput key ->
            match key.Key with
            | Key.Up -> update Up model
            | Key.Down -> update Down model
            | Key.PageUp -> update PageUp model
            | Key.PageDown -> update PageDown model
            | Key.Home -> update Top model
            | Key.End -> update Bottom model
            | _ -> model, Cmd.none
        | MouseInput mouse ->
            match mouse.Button with
            | MouseButton.ScrollUp -> update Up model
            | MouseButton.ScrollDown -> update Down model
            | _ -> model, Cmd.none

    let setRows rows (model: Model<'row>) = scrolled { model with Rows = rows }

    let setSize width height (model: Model<'row>) =
        scrolled
            { model with
                Width = width
                Height = height }

    let selectedRow (model: Model<'row>) =
        if List.isEmpty model.Rows then
            None
        else
            Some(List.item (clampCursor model) model.Rows)

    let private columnWidths (model: Model<'row>) =
        let titles = model.Columns |> List.map (fun c -> Width.ofString c.Title)

        if model.Width <= 0 then
            List.map2
                (fun (c: Column) title ->
                    match c.Width with
                    | Len n
                    | Min n -> max 0 n
                    | _ -> title)
                model.Columns
                titles
        else
            let gaps = max 0 (List.length model.Columns - 1)
            Solver.solve (model.Columns |> List.map (fun (c: Column) -> c.Width)) titles (max 0 (model.Width - gaps))

    let private space n : Run list =
        if n > 0 then
            [ { Text = String.replicate n " "
                Style = Style.Default } ]
        else
            []

    /// One row's runs: each cell cut and padded to its column, one space between
    /// columns. Missing cells are blank and extra cells are dropped.
    let private rowRuns (model: Model<'row>) (widths: int list) (cells: Node list) =
        let runs =
            widths
            |> List.mapi (fun i width ->
                let cell =
                    match List.tryItem i cells with
                    | Some node -> Runs.ofNode Style.Default node |> Runs.truncate width
                    | None -> []

                (if i > 0 then space 1 else []) @ cell @ space (width - Runs.width cell))
            |> List.concat

        // The gaps alone can be wider than a very small table.
        if model.Width > 0 then
            Runs.truncate model.Width runs
        else
            runs

    // The style sits on the line, so a background fills the whole row while each
    // cell keeps its own colours on top.
    let private line style (runs: Run list) =
        Ui.line (runs |> List.map (fun run -> Ui.text run.Text |> Ui.style run.Style))
        |> Ui.style style

    let view (model: Model<'row>) : Node =
        if List.isEmpty model.Columns then
            Ui.col []
        else
            let m = scrolled model
            let widths = columnWidths m
            let height = max 0 m.Height

            let header =
                m.Columns
                |> List.map (fun c -> Ui.text c.Title)
                |> rowRuns m widths
                |> line m.HeaderStyle

            let shown =
                m.Rows
                |> List.skip m.Offset
                |> List.truncate height
                |> List.mapi (fun i row ->
                    let style =
                        if m.Offset + i = m.Cursor then
                            m.SelectedStyle
                        else
                            m.NormalStyle

                    line style (rowRuns m widths (m.RowToCells row)))

            Ui.col (header :: shown @ List.replicate (height - shown.Length) (Ui.text ""))
