namespace Fansi

open Elmish
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module CheckboxComponent =

    type Message = | Toggle

    type Model =
        { Checked: bool
          Label: string
          CheckedChar: string
          UncheckedChar: string
          CheckedStyle: Style
          UncheckedStyle: Style }

    let init label =
        { Checked = false
          Label = label
          CheckedChar = "☑"
          UncheckedChar = "☐"
          CheckedStyle =
            { Style.Default with
                FgColor = Color.Green
                Bold = true }
          UncheckedStyle = Style.Default },
        Cmd.none

    let update msg model =
        match msg with
        | Toggle ->
            { model with
                Checked = not model.Checked },
            Cmd.none

    let view model : Node =
        let checkMark =
            if model.Checked then
                model.CheckedChar
            else
                model.UncheckedChar

        let style =
            if model.Checked then
                model.CheckedStyle
            else
                model.UncheckedStyle

        Ui.row
            [ Ui.text $"{checkMark} " |> Ui.style style
              Ui.text model.Label |> Ui.style style ]
