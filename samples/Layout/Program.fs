module Fansi.Samples.Layout

open Elmish
open Fansi
open Fansi.Core

type Page =
    { Title: string
      Note: string
      Demo: Node }

let private box label =
    Ui.col [ Ui.text label ] |> Ui.border Single |> Ui.padX 1

let pages =
    [ { Title = "Len and Fill"
        Note = "len 12 keeps its width. fill 1 takes what is left."
        Demo = Ui.row [ box "len 12" |> Ui.len 12; box "fill 1" |> Ui.fill 1 ] }

      { Title = "Fill weights"
        Note = "Fill children share the space by weight: here a quarter, a half and a quarter."
        Demo =
          Ui.row
              [ box "fill 1" |> Ui.fill 1
                box "fill 2" |> Ui.fill 2
                box "fill 1" |> Ui.fill 1 ] }

      { Title = "Pct and Ratio"
        Note = "pct 25 takes a quarter of the width, ratio 1 3 a third. fill 1 gets the rest."
        Demo =
          Ui.row
              [ box "pct 25" |> Ui.pct 25
                box "ratio 1 3" |> Ui.ratio 1 3
                box "fill 1" |> Ui.fill 1 ] }

      { Title = "Min and Max"
        Note = "minLen 20 starts at 20 and grows like fill 1. maxLen fits its content, up to a cap."
        Demo =
          Ui.row
              [ box "minLen 20" |> Ui.minLen 20
                box "maxLen 30" |> Ui.maxLen 30
                box "maxLen 16 cuts this longer label" |> Ui.maxLen 16
                box "fill 1" |> Ui.fill 1 ] }

      { Title = "Auto"
        Note = "Auto is the default. A child is as big as its content."
        Demo = Ui.row [ box "short"; box "a longer label"; box "fill 1" |> Ui.fill 1 ] }

      { Title = "Shrinking"
        Note = "Three len 30 boxes want 90 columns. When there is less, the last box shrinks first."
        Demo =
          Ui.row
              [ box "first" |> Ui.len 30
                box "second" |> Ui.len 30
                box "third" |> Ui.len 30 ] }

      { Title = "Justify"
        Note = "Justify places the space left over along the main axis."
        Demo =
          Ui.col
              [ for name, justify in
                    [ "start", Justify.Start
                      "center", Justify.Center
                      "end", Justify.End
                      "between", Justify.Between ] ->
                    Ui.row [ box name |> Ui.len 12; box "a" |> Ui.len 5; box "b" |> Ui.len 5 ]
                    |> Ui.justify justify
                    |> Ui.len 3 ] }

      { Title = "Align"
        Note = "Align places a child across the main axis. Stretch, the default, fills it."
        Demo =
          Ui.col
              [ for name, align in
                    [ "start", Align.Start
                      "center", Align.Center
                      "end", Align.End
                      "stretch", Align.Stretch ] -> Ui.col [ box name ] |> Ui.align align |> Ui.len 3 ] }

      { Title = "Cross"
        Note = "cross sets the size across the axis, and beats align."
        Demo =
          Ui.row
              [ box "cross (Len 5)" |> Ui.cross (Len 5) |> Ui.fill 1
                box "cross (Pct 50)" |> Ui.cross (Pct 50) |> Ui.fill 1
                box "stretch" |> Ui.fill 1 ] }

      { Title = "Borders"
        Note = "Five border styles, and borders nest."
        Demo =
          Ui.col
              [ Ui.row
                    [ for name, border in
                          [ "single", Single
                            "double", Double
                            "rounded", Rounded
                            "heavy", Heavy
                            "ascii", Ascii ] -> Ui.col [ Ui.text name ] |> Ui.border border |> Ui.padX 1 |> Ui.fill 1 ]
                |> Ui.len 3
                Ui.col
                    [ Ui.col [ Ui.col [ Ui.text "three deep" ] |> Ui.border Single |> Ui.fill 1 ]
                      |> Ui.border Double
                      |> Ui.pad 1
                      |> Ui.fill 1 ]
                |> Ui.border Heavy
                |> Ui.fill 1 ] }

      { Title = "Padding and margin"
        Note = "Padding is inside the border and takes the background. Margin is outside it."
        Demo =
          Ui.row
              [ box "pad 2" |> Ui.pad 2 |> Ui.bg Color.Blue |> Ui.fill 1
                box "margin 2" |> Ui.margin 2 |> Ui.bg Color.Blue |> Ui.fill 1 ] }

      { Title = "Text"
        Note = "Ui.line mixes styles in one row. Wide characters take two cells."
        Demo =
          Ui.col
              [ Ui.line
                    [ Ui.text "Status: "
                      Ui.text "ok" |> Ui.bold |> Ui.fg Color.Green
                      Ui.text " - 3 warnings" |> Ui.fg Color.Yellow ]
                |> Ui.len 1
                Ui.text "日本語のテキスト and emoji 😀 line up" |> Ui.len 1
                Ui.col [ Ui.text "A box with its title in the border." ]
                |> Ui.border Rounded
                |> Ui.title "Title"
                |> Ui.padX 1
                |> Ui.fill 1 ] } ]

type Model = { Page: int }

let init () = { Page = 0 }, Cmd.none

let private go delta model =
    { model with
        Page = (model.Page + delta + pages.Length) % pages.Length }

let update (msg: FansiMsg<unit>) (model: Model) =
    match msg with
    | KeyPress k ->
        match k.Key with
        | Key.Esc -> model, Cmd.quit
        | Key.Tab when k.Shift -> go (-1) model, Cmd.none
        | Key.Right
        | Key.Tab -> go 1 model, Cmd.none
        | Key.Left -> go (-1) model, Cmd.none
        | Key.Home -> { model with Page = 0 }, Cmd.none
        | Key.End -> { model with Page = pages.Length - 1 }, Cmd.none
        | _ -> model, Cmd.none
    | _ -> model, Cmd.none

let view (model: Model) =
    let page = pages[model.Page]

    Ui.col
        [ Ui.text $"{model.Page + 1}/{pages.Length}  {page.Title}"
          |> Ui.bold
          |> Ui.fg Color.Cyan
          |> Ui.len 1
          // Two rows, so a note still reads on a narrow terminal where it wraps.
          Ui.text page.Note |> Ui.len 2
          page.Demo |> Ui.fill 1
          Ui.text "left / right: page - home / end: first / last - esc: quit - resize to watch"
          |> Ui.fg Color.BrightBlack
          |> Ui.italic
          |> Ui.len 1 ]

[<EntryPoint>]
let main _ =
    FansiProgram.mkProgram init update view |> FansiProgram.run
    0
