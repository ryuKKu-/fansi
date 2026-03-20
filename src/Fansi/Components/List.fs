namespace Fansi

open System
open Elmish
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
          Selected: int
          ViewportOffset: int
          ViewportSize: int
          Focused: bool
          ItemToString: 'item -> string
          SelectedStyle: Style
          NormalStyle: Style
          FocusedIndicator: string }

    let init (items: 'item list) (itemToString: 'item -> string) (viewportSize: int) =
        { Items = items
          Selected = 0
          ViewportOffset = 0
          ViewportSize = viewportSize
          Focused = false
          ItemToString = itemToString
          SelectedStyle = { Style.Default with FgColor = Color.Black; BgColor = Color.Cyan; Bold = true }
          NormalStyle = Style.Default
          FocusedIndicator = "▸ " },
        Cmd.none

    let rec update msg model =
        match msg with
        | MoveUp ->
            let newSel = max 0 (model.Selected - 1)
            let offset =
                if newSel < model.ViewportOffset then newSel
                else model.ViewportOffset
            { model with Selected = newSel; ViewportOffset = offset }, Cmd.none

        | MoveDown ->
            let newSel = min (model.Items.Length - 1) (model.Selected + 1)
            let offset =
                if newSel >= model.ViewportOffset + model.ViewportSize then
                    newSel - model.ViewportSize + 1
                else model.ViewportOffset
            { model with Selected = newSel; ViewportOffset = offset }, Cmd.none

        | Select -> model, Cmd.none

        | KeyInput cki when model.Focused ->
            match cki.Key with
            | ConsoleKey.UpArrow -> update MoveUp model
            | ConsoleKey.DownArrow -> update MoveDown model
            | ConsoleKey.Enter -> update Select model
            | _ -> model, Cmd.none

        | KeyInput _ -> model, Cmd.none

    let selectedItem model =
        if model.Selected >= 0 && model.Selected < model.Items.Length then
            Some model.Items[model.Selected]
        else None

    let view (model: Model<'item>) : Node =
        let visibleItems =
            model.Items
            |> List.skip model.ViewportOffset
            |> List.truncate model.ViewportSize

        let rows =
            visibleItems
            |> List.mapi (fun i item ->
                let idx = i + model.ViewportOffset
                let isSelected = idx = model.Selected
                let style = if isSelected then model.SelectedStyle else model.NormalStyle
                let prefix = if isSelected then model.FocusedIndicator else String.replicate model.FocusedIndicator.Length " "
                Node.styledText style $"{prefix}{model.ItemToString item}")

        Node.column rows
