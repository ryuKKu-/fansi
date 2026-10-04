# Fansi

Terminal user interfaces in F#, with the Elm architecture.

You write a model, an `update` and a `view`. Fansi puts the terminal in raw
mode, turns keys, mouse, paste and resize into messages, lays out the view with
integer constraints and repaints only the cells that changed.

## Install

```shell
dotnet add package Fansi.Tui
```

The package is `Fansi.Tui`, and the namespace is `Fansi`:

```fsharp
open Fansi
```

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

- Constructors: `Ui.text`, `Ui.line`, `Ui.row`, `Ui.col`, `Ui.empty`.
- Size along the parent's axis: `Ui.len`, `Ui.pct`, `Ui.ratio`, `Ui.fill`,
  `Ui.minLen`, `Ui.maxLen`, `Ui.auto`. The sizes always add up to the
  parent's size exactly, with no cell lost to rounding.
- Box: `Ui.border`, `Ui.title`, `Ui.titleWith`, `Ui.pad`, `Ui.padX`, `Ui.padY`, `Ui.margin`,
  `Ui.justify`, `Ui.align`.
- Style: `Ui.fg`, `Ui.bg`, `Ui.bold`, `Ui.italic`, `Ui.underline`,
  `Ui.strike`, `Ui.style`.

`Ui.line` draws its children as one run of text, each part in its own style:

```fsharp
Ui.line [ Ui.text "Status: "; Ui.text "ok" |> Ui.bold |> Ui.fg Color.Green ]
```

Inside a line only text and style count. A row or column in it gives its text,
and its border and padding are ignored. Text is measured in terminal cells, so
CJK characters and emoji take two.

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
| `ViewportComponent` | a box of rich text that scrolls, wraps and follows a growing log |
| `CheckboxComponent` | a toggle with a label |
| `ButtonComponent` | a label in a box |
| `TimerComponent` | a countdown |
| `SpinnerComponent` | an animated spinner |
| `ProgressBarComponent` | a bar filled in proportion |

A component is a model, an `update`, a `view`, and a `subscribe` if it ticks.
The parent owns the model and routes messages to it. `TextInputComponent`
takes keys as `KeyInput`, pastes as `Pasted` and programmatic changes as
`SetValue`.

`ViewportComponent` shows `Height` rows of its content, wrapped to `Width`
cells. The content is one node per paragraph, so every style works:

```fsharp
let help, _ =
    ViewportComponent.init 40 10 [ Ui.text "Keys" |> Ui.bold; Ui.text "Up and Down scroll." ]
```

`setContent` and `setText` replace the content. `setText` makes one paragraph
per line and drops one trailing newline, so `"a\nb\n"` gives two paragraphs. A
viewport at the bottom stays at the bottom, so a log follows new lines.
Content that fits the box counts as both the top and the bottom, so new content
or a smaller size then shows the end. To show a document from its start, load
it through `init`, and send `Top` after `setContent`, `setText` or `setSize`.
Call `setSize` when the terminal is resized.

Set `Width` and `Height` to the inside of the box you put the viewport in. A
narrower or shorter box wraps or cuts rows again. A paragraph's own size,
border and padding are ignored, and its background colours only its text, not
the whole row. Tabs and other control characters are dropped, so expand tabs
before passing the text in.

## Samples

- `samples/Layout`: one page per layout idea. Left and Right change page.
- `samples/Dashboard`: three panels, real components and a focus ring. Its
  README has the dashboard checklist.
- `samples/Components`: every component on one screen, with a help box that
  scrolls. Its README has the component checklist.
- `samples/Interactive`: raw input, one event at a time. Its README has the
  terminal checklist.

```
dotnet run --project samples/Dashboard
```

## Project layout

The library lives in `src/Fansi`, one folder per concern:

| Folder | What it holds |
|---|---|
| `Core` | geometry, styles, props and the node tree |
| `Text` | display width and styled runs of text |
| `Ui.fs` | the functions that build a view |
| `Layout` | the constraint solver and the layout pass |
| `Rendering` | the cell buffer, painting and the diffing renderer |
| `Terminal` | input events, the input parser and raw mode |
| `Program` | the Elm loop, subscriptions, keymaps, focus and the cursor |
| `Components` | ready-made components |

`tests/Fansi.Tests` uses the same folders, plus `Samples` for the sample tests.

## Building

```
dotnet tool restore
dotnet paket restore
dotnet build
dotnet test tests/Fansi.Tests
dotnet fantomas src tests samples
```

The build has no warnings, and it should stay that way.

## Contributing

Commit messages are one line in the
[conventional commits](https://www.conventionalcommits.org) style:
`feat: ...`, `fix: ...`, `perf: ...`, `docs: ...`, `refactor: ...`,
`test: ...`, `chore: ...`. Releases and the changelog are worked out from
them: `feat`, `fix` and `perf` make a release, the others do not.

## Licence

MIT. See [LICENSE](https://github.com/ryuKKu-/fansi/blob/master/LICENSE).
