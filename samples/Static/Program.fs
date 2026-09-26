module Fansi.Samples.Static

open System
open System.IO
open Fansi
open Fansi.Core

let private title text =
    Ui.text text |> Ui.bold |> Ui.fg Color.Cyan

let private panel heading body =
    Ui.col [ title heading; Ui.text ""; body ]
    |> Ui.border Rounded
    |> Ui.padX 1
    |> Ui.fill 1

/// The three-panel dashboard the layout rewrite was for: nested borders, three
/// panels sharing the width, and a status line pinned to one row.
let dashboard () =
    Ui.col
        [ Ui.row
              [ panel "Timer" (Ui.text "00:01:30.000")
                panel "Input" (Ui.text "> hello")
                panel "Tasks" (Ui.text "- compile\n- test\n- deploy") ]
          |> Ui.fill 1

          Ui.text "static sample - renders once and exits"
          |> Ui.fg Color.BrightBlack
          |> Ui.italic
          |> Ui.len 1 ]

[<EntryPoint>]
let main _ =
    // WindowWidth/WindowHeight throw IOException when there's no real console attached
    // (piped output, CI, this sample run from a non-interactive shell). Fall back to a
    // fixed size so the sample still renders something instead of crashing.
    let width, height =
        try
            max 40 Console.WindowWidth, max 10 (Console.WindowHeight - 1)
        with :? IOException ->
            80, 23

    // Flush only writes cells that differ from a blank previous frame, and this
    // sample draws once onto whatever is already on the primary screen, so without
    // clearing first, old content would show through the panels' blank interiors.
    Console.Out.Write AnsiSequence.clearScreen

    let renderer = Renderer.Renderer(30<FPS>, Console.Out)
    renderer.SetFrame(Paint.render width height (dashboard ()))
    renderer.Flush()

    Console.Out.Write AnsiSequence.resetStyle
    Console.Out.WriteLine()
    0
