namespace Fansi

open System
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module HelpComponent =

    type Model =
        {
            ShowAll: bool
            /// Cells. 0 or below means no limit.
            Width: int
            Separator: string
            Ellipsis: string
            KeyStyle: Style
            DescriptionStyle: Style
            SeparatorStyle: Style
        }

    let init () =
        { ShowAll = false
          Width = 0
          Separator = " • "
          Ellipsis = "…"
          KeyStyle = { Style.Default with Bold = true }
          DescriptionStyle =
            { Style.Default with
                FgColor = Color.BrightBlack }
          SeparatorStyle =
            { Style.Default with
                FgColor = Color.BrightBlack } }

    let toggle model =
        { model with
            ShowAll = not model.ShowAll }

    let private columnGap = 3

    let private room model =
        if model.Width <= 0 then Int32.MaxValue else model.Width

    let private entries (bindings: Keymap.KeyBind list) =
        bindings |> List.choose (fun b -> if b.Enabled then b.Help else None)

    let private styled style text = Ui.text text |> Ui.style style

    /// Takes items from the front while they fit, with `gap` cells between two
    /// items. Returns the cells used, the items taken, and whether any items remain.
    let private fitting room gap (widthOf: 'a -> int) (items: 'a list) =
        let rec go used taken rest =
            match rest with
            | item :: tail ->
                let needed = (if List.isEmpty taken then 0 else gap) + widthOf item

                if used + needed <= room then
                    go (used + needed) (item :: taken) tail
                else
                    used, List.rev taken, true
            | [] -> used, List.rev taken, false

        go 0 [] items

    // Whole entries only. Half an entry would show a key without its action.
    let private ellipsisAfter model used cut =
        let more = " " + model.Ellipsis

        if cut && used + Width.ofString more <= room model then
            [ styled model.SeparatorStyle more ]
        else
            []

    let private shortHelp model (bindings: Keymap.KeyBind list) =
        let widthOf (h: Keymap.Help) =
            Width.ofString h.Key + 1 + Width.ofString h.Description

        match fitting (room model) (Width.ofString model.Separator) widthOf (entries bindings) with
        | _, [], _ -> Ui.text ""
        | used, shown, cut ->
            let parts =
                shown
                |> List.mapi (fun i (h: Keymap.Help) ->
                    [ if i > 0 then
                          styled model.SeparatorStyle model.Separator
                      styled model.KeyStyle h.Key
                      styled model.DescriptionStyle (" " + h.Description) ])
                |> List.concat

            Ui.line (parts @ ellipsisAfter model used cut)

    let private column model (group: Keymap.Help list) =
        let keyWidth = group |> List.map (fun h -> Width.ofString h.Key) |> List.max

        let descriptionWidth =
            group |> List.map (fun h -> Width.ofString h.Description) |> List.max

        let line (h: Keymap.Help) =
            Ui.line
                [ styled model.KeyStyle (h.Key + String.replicate (keyWidth - Width.ofString h.Key) " ")
                  styled model.DescriptionStyle ("  " + h.Description) ]

        keyWidth + 2 + descriptionWidth, Ui.col (List.map line group)

    let private fullHelp model (groups: Keymap.KeyBind list list) =
        let columns =
            groups
            |> List.map entries
            |> List.filter (not << List.isEmpty)
            |> List.map (column model)

        match fitting (room model) columnGap fst columns with
        | _, [], _ -> Ui.text ""
        | used, shown, cut ->
            let body =
                shown
                |> List.mapi (fun i (_, node) ->
                    if i = 0 then
                        [ node ]
                    else
                        [ Ui.text (String.replicate columnGap " "); node ])
                |> List.concat

            Ui.row (body @ ellipsisAfter model used cut)

    let view (short: Keymap.KeyBind list) (full: Keymap.KeyBind list list) model : Node =
        if model.ShowAll then
            fullHelp model full
        else
            shortHelp model short
