# Fansi

Terminal user interfaces in F#, with the Elm architecture.

You write a model, an `update` and a `view`. Fansi puts the terminal in raw
mode, turns keys, mouse, paste and resize into messages, lays out the view with
integer constraints and repaints only the cells that changed.

## A counter

```fsharp
open Elmish
open Fansi
open Fansi.Core

type Model = { Count: int }

let init () = { Count = 0 }, Cmd.none

let update msg model =
    match msg with
    | KeyPress k when k.Key = Key.Esc -> model, Cmd.quit
    | KeyPress k when k.Key = Key.Up -> { Count = model.Count + 1 }, Cmd.none
    | KeyPress k when k.Key = Key.Down -> { Count = model.Count - 1 }, Cmd.none
    | _ -> model, Cmd.none

let view model =
    Ui.col
        [ Ui.text $"count: {model.Count}" |> Ui.bold |> Ui.len 1
          Ui.text "up / down to change, esc to quit" |> Ui.fg Color.BrightBlack ]
    |> Ui.border Rounded
    |> Ui.padX 1

[<EntryPoint>]
let main _ =
    FansiProgram.mkProgram init update view |> FansiProgram.run
    0
```

Open `Elmish` before `Fansi`. Fansi adds to Elmish's `Cmd` and `Sub` modules,
and the module opened last wins where both define the same name.

## Messages

`update` receives a `FansiMsg<'msg>` and returns a plain `Cmd<'msg>`:

| Case | When |
|---|---|
| `KeyPress of KeyEvent` | a key, with `Ctrl`, `Alt` and `Shift` flags |
| `Mouse of MouseEvent` | a click, a scroll or a move, once `withMouseEnabled` is on |
| `Paste of string` | a whole bracketed paste, as one message |
| `Resize of width * height` | the terminal changed size |
| `FocusChanged of bool` | the terminal window gained or lost focus |
| `App of 'msg` | your own messages, from commands and subscriptions |

Your commands come back as `App`, so a child component's command needs only
`Cmd.map`. `Cmd.quit` stops the program and `Cmd.after 500<ms> msg` sends a
message once, later. Ctrl+C quits unless you use `withoutQuitOnCtrlC`.

## Views

Constructors return a `Node` and every modifier is `Node -> Node`, so you can
adjust a node a component already built:

```fsharp
TimerComponent.view model.Timer |> Ui.border Single |> Ui.fill 1
```

- Constructors: `Ui.text`, `Ui.row`, `Ui.col`, `Ui.empty`.
- Size along the parent's axis: `Ui.len`, `Ui.pct`, `Ui.ratio`, `Ui.fill`,
  `Ui.minLen`, `Ui.maxLen`, `Ui.auto`. The sizes always add up to the
  parent's size exactly, with no cell lost to rounding.
- Box: `Ui.border`, `Ui.pad`, `Ui.padX`, `Ui.padY`, `Ui.margin`,
  `Ui.justify`, `Ui.align`.
- Style: `Ui.fg`, `Ui.bg`, `Ui.bold`, `Ui.italic`, `Ui.underline`,
  `Ui.strike`, `Ui.style`.

## Focus

A `Focus` ring holds which part of the screen has focus. Components do not
store it. You pass it in:

```fsharp
Focus = Focus.ofList [ Name; Password; Submit ]

| KeyPress k when k.Key = Key.Tab -> { model with Focus = Focus.next model.Focus }, Cmd.none

TextInputComponent.view (Focus.isFocused Name model.Focus) model.Name
```

## Subscriptions

Components that tick, such as the timer, the spinner and the text cursor,
carry their own id. Their subscriptions never collide, so `Sub.map` needs no
key:

```fsharp
let subscribe model =
    Sub.batch
        [ TimerComponent.subscribe model.Timer |> Sub.map TimerMsg
          TextInputComponent.subscribe (Focus.isFocused Name model.Focus) model.Name
          |> Sub.map NameMsg ]

FansiProgram.mkProgram init update view
|> FansiProgram.withSubscription subscribe
|> FansiProgram.run
```

For a timer of your own, `Sub.timer [ "clock" ] 1000<ms> Tick`.

## Components

| Module | What it does |
|---|---|
| `TextInputComponent` | single-line input: scrolling, password echo, placeholder, undo, validation, suggestions |
| `ListComponent` | a scrolling list with a marker and a selection |
| `CheckboxComponent` | a toggle with a label |
| `ButtonComponent` | a label in a box |
| `TimerComponent` | a countdown |
| `SpinnerComponent` | an animated spinner |
| `ProgressBarComponent` | a bar filled in proportion |

A component is a model, an `update`, a `view`, and a `subscribe` if it ticks.
The parent owns the model and routes messages to it. `TextInputComponent`
takes keys as `KeyInput`, pastes as `Pasted` and programmatic changes as
`SetValue`.

## Samples

- `samples/Static`: renders a layout once and exits.
- `samples/Interactive`: raw input, one event at a time. Its README has the
  terminal checklist.
- `samples/Components`: every component on one screen. Its README has the
  component checklist.

```
dotnet run --project samples/Components
```

## Building

```
dotnet tool restore
dotnet paket restore
dotnet build
dotnet test tests/Fansi.Tests
```
