namespace Fansi

open Elmish
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module ButtonComponent =

    type Message = | Pressed

    type Model =
        { Label: string
          Style: Style
          FocusedStyle: Style }

    let init label =
        { Label = label
          Style =
            { Style.Default with
                FgColor = Color.White
                BgColor = Color.BrightBlack }
          FocusedStyle =
            { Style.Default with
                FgColor = Color.Black
                BgColor = Color.Cyan
                Bold = true } },
        Cmd.none

    let update msg model =
        match msg with
        | Pressed -> model, Cmd.none

    let view (focused: bool) model : Node =
        let style = if focused then model.FocusedStyle else model.Style
        let label = $" {model.Label} "

        Ui.row [ Ui.text label |> Ui.style style ]
        |> Ui.style style
        |> Ui.padX 1
        |> Ui.border (if focused then Rounded else Single)
