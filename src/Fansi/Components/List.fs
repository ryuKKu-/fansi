namespace Fansi

open System
open Elmish
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module ListComponent =

    type Message =
        | MoveUp
        | MoveDown
        | Select
        | KeyInput of ConsoleKeyInfo

    type Model<'item> =
        { Items: 'item list
          FocusItemIndex: int
          SelectedItemIndex: int option
          ViewportOffset: int
          ViewportSize: int
          Focused: bool
          ItemToString: 'item -> string
          SelectedStyle: Style
          NormalStyle: Style
          FocusedStyle: Style
          FocusedIndicator: string }

    let init (items: 'item list) (itemToString: 'item -> string) (viewportSize: int) =
        { Items = items
          FocusItemIndex = 0
          SelectedItemIndex = None
          ViewportOffset = 0
          ViewportSize = viewportSize
          Focused = false
          ItemToString = itemToString
          SelectedStyle =
            { Style.Default with
                FgColor = Color.Red
                BgColor = Color.Black
                Bold = true }
          NormalStyle = Style.Default
          FocusedStyle =
            { Style.Default with
                FgColor = Color.Black
                BgColor = Color.Cyan }
          FocusedIndicator = "▸ " },
        Cmd.none

    let rec update msg model =
        match msg with
        | MoveUp ->
            let newSel = max 0 (model.FocusItemIndex - 1)

            let offset =
                if newSel < model.ViewportOffset then
                    newSel
                else
                    model.ViewportOffset

            { model with
                FocusItemIndex = newSel
                ViewportOffset = offset },
            Cmd.none

        | MoveDown ->
            let newSel = min (model.Items.Length - 1) (model.FocusItemIndex + 1)

            let offset =
                if newSel >= model.ViewportOffset + model.ViewportSize then
                    newSel - model.ViewportSize + 1
                else
                    model.ViewportOffset

            { model with
                FocusItemIndex = newSel
                ViewportOffset = offset },
            Cmd.none

        | Select ->
            let selectedIndex =
                match model.SelectedItemIndex, model.FocusItemIndex with
                | None, idx -> Some idx
                | Some idx, i -> if idx = i then None else Some i

            { model with
                SelectedItemIndex = selectedIndex },
            Cmd.none

        | KeyInput cki when model.Focused ->
            match cki.Key with
            | ConsoleKey.UpArrow -> update MoveUp model
            | ConsoleKey.DownArrow -> update MoveDown model
            | ConsoleKey.Enter -> update Select model
            | _ -> model, Cmd.none

        | KeyInput _ -> model, Cmd.none

    let selectedItem model =
        model.SelectedItemIndex |> Option.map (fun idx -> model.Items[idx])

    let view (model: Model<'item>) : Node =
        let visibleItems =
            model.Items
            |> List.skip model.ViewportOffset
            |> List.truncate model.ViewportSize

        let rows =
            visibleItems
            |> List.mapi (fun i item ->
                let idx = i + model.ViewportOffset
                let isFocused = idx = model.FocusItemIndex
                let isSelected = model.SelectedItemIndex |> Option.contains idx

                let style =
                    match isSelected, isFocused with
                    | true, _ -> model.SelectedStyle
                    | false, true -> model.FocusedStyle
                    | false, false -> model.NormalStyle

                let prefix =
                    if isFocused then
                        model.FocusedIndicator
                    else
                        String.replicate model.FocusedIndicator.Length " "

                Ui.text $"{prefix}{model.ItemToString item}" |> Ui.style style)

        Ui.col rows
