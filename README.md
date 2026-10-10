# Fansi

**Terminal apps in F#, built the Elm way.**

[![NuGet](https://img.shields.io/nuget/v/Fansi.Tui.svg)](https://www.nuget.org/packages/Fansi.Tui)
[![CI](https://github.com/ryuKKu-/fansi/actions/workflows/ci.yml/badge.svg)](https://github.com/ryuKKu-/fansi/actions/workflows/ci.yml)
[![Licence: MIT](https://img.shields.io/badge/licence-MIT-blue.svg)](https://github.com/ryuKKu-/fansi/blob/master/LICENSE)

Fansi is a library for building full-screen terminal apps in F#. It follows the Elm architecture through
[Elmish](https://elmish.github.io/elmish/): your app is a model, an `update` function that changes it, and a `view`
function that describes the screen. Fansi does the rest: it reads the keyboard and the mouse, lays out the view, and
redraws only the parts of the screen that changed.

- **Plain F# views.** A view is a tree of rows, columns and text. Sizes, borders, padding and colours are small
  functions you pipe together.
- **Exact layout.** A constraint solver splits space in whole terminal cells. Wide characters such as CJK and emoji
  take two cells and are never cut in half.
- **Ready-made components.** Text input, list, table, viewport, help line, progress bar, spinner, timer and more,
  each one a small Elm program you can restyle.
- **Real terminal input.** Keys with modifiers, mouse, bracketed paste, resize and focus events, on Unix and
  Windows.
- **Safe exit.** The terminal goes back to normal however the program ends, even on a crash.

### Requirements

[.NET 10](https://dotnet.microsoft.com/download).

## Getting started

```shell
dotnet add package Fansi.Tui
```

A complete counter. Up and Down change the number, and Esc quits:

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

Open `Elmish` before `Fansi`. Fansi adds functions to the `Cmd` and `Sub` modules of Elmish, and when both define a
name, the module you open last wins.

## See it run

The repository has five samples. Clone it and run one:

```shell
dotnet run --project samples/Dashboard
```

| Sample | What it shows |
|---|---|
| `samples/Layout` | one page for each layout idea. Left and Right change the page |
| `samples/Dashboard` | three panels, real components and a focus ring |
| `samples/Components` | every component on one screen |
| `samples/Table` | a table of processes with a help line |
| `samples/Interactive` | raw input, one event at a time |

## Learn more

- [Architecture](https://github.com/ryuKKu/fansi/blob/master/docs/architecture.md): how Fansi works, from the Elm
  loop and input parsing to layout, painting and rendering.
- [Component guide](https://github.com/ryuKKu-/fansi/blob/master/docs/components.md): the ten ready-made components,
  with an example for each.
- [Contributing](https://github.com/ryuKKu-/fansi/blob/master/CONTRIBUTING.md): build, test and send a change.

## Licence

MIT. See [LICENSE](https://github.com/ryuKKu-/fansi/blob/master/LICENSE).
