namespace Fansi

open System
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module ProgressBarComponent =

    type Model =
        { Progress: float
          Width: int
          FilledChar: char
          EmptyChar: char
          FilledStyle: Style
          EmptyStyle: Style
          ShowPercentage: bool }

    let init width =
        { Progress = 0.0
          Width = width
          FilledChar = '█'
          EmptyChar = '░'
          FilledStyle =
            { Style.Default with
                FgColor = Color.Green }
          EmptyStyle =
            { Style.Default with
                FgColor = Color.BrightBlack }
          ShowPercentage = true }

    let setProgress progress model =
        { model with
            Progress = Math.Clamp(progress, 0.0, 1.0) }

    let view (model: Model) : Node =
        let filledCount = int (float model.Width * model.Progress)
        let emptyCount = model.Width - filledCount
        let filled = String.replicate filledCount (string model.FilledChar)
        let empty = String.replicate emptyCount (string model.EmptyChar)

        let bar =
            Ui.row
                [ Ui.text filled |> Ui.style model.FilledStyle
                  Ui.text empty |> Ui.style model.EmptyStyle ]

        if model.ShowPercentage then
            let pct = $" {int (model.Progress * 100.0)}%%"
            Ui.row [ bar; Ui.text pct ]
        else
            bar
