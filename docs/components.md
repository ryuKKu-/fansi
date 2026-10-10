# Components

Fansi comes with ten ready-made components. They all work the same way, so once you have used one, you know the rest.

## How a component works

A component is a plain model with an `update` and a `view`. Components that tick (a timer, a spinner, a blinking cursor) also have a `subscribe`.

Your program owns the model. It stores the component model in its own model, forwards messages to the component's `update`, and calls the component's `view` when it draws.

```fsharp
type Msg = NameMsg of TextInputComponent.Message

// init
let name, _ = TextInputComponent.init ()

// update
| App(NameMsg m) ->
    let name, cmd = TextInputComponent.update m model.Name
    { model with Name = name }, Cmd.map NameMsg cmd

// view
TextInputComponent.view (Focus.isFocused Name model.Focus) model.Name
```

A few things hold for every component:

- **Settings are record fields.** Create the model with `init`, then change what you want with `{ model with ... }`. For example `{ name with Placeholder = "your name" }`.
- **Components never own focus.** `view` takes a `focused` flag where focus changes the look. You decide who has focus. See `Focus` in the main README.
- **`init` can return a command.** Most `init` functions return `model, cmd`. `ListComponent.init` and `ProgressBarComponent.init` return only the model.
- **Keys come in as `KeyInput`.** Forward the `KeyEvent` from `KeyPress` and the component works out what it means.
- **A view is a `Node`.** You can wrap it with any `Ui` modifier, such as `Ui.border` or `Ui.len`.

The [Components sample](https://github.com/ryuKKu-/fansi/tree/master/samples/Components) uses all of them on one screen.

## Overview

| Component | Use it for | Ticks |
|---|---|---|
| [`TextInputComponent`](#textinputcomponent) | one line of typed text | cursor blink |
| [`ListComponent`](#listcomponent) | choosing from a short list | no |
| [`TableComponent`](#tablecomponent) | rows and columns with a cursor | no |
| [`ViewportComponent`](#viewportcomponent) | long text or a log that scrolls | no |
| [`HelpComponent`](#helpcomponent) | a line of key hints | no |
| [`CheckboxComponent`](#checkboxcomponent) | an on/off choice | no |
| [`ButtonComponent`](#buttoncomponent) | an action | no |
| [`TimerComponent`](#timercomponent) | a countdown | yes |
| [`SpinnerComponent`](#spinnercomponent) | "something is happening" | yes |
| [`ProgressBarComponent`](#progressbarcomponent) | how far a task has got | no |

## TextInputComponent

A single-line input. It scrolls sideways when the text is wider than the box, and it supports a password mode, a placeholder, undo and redo, validation, and suggestions.

```fsharp
let name, _ = TextInputComponent.init ()

let name =
    { name with
        Width = 20
        Placeholder = "your name"
        Suggestions = [ "Ada Lovelace"; "Alan Turing" ] }
    |> TextInputComponent.withValidation (fun s ->
        if System.String.IsNullOrWhiteSpace s then
            TextInputComponent.Invalid "a name is required"
        else
            TextInputComponent.Valid)
```

| Message | What it does |
|---|---|
| `KeyInput of KeyEvent` | edits the text from a key press |
| `Pasted of string` | inserts pasted text (forward `Paste` here) |
| `SetValue of string` | replaces the text from your code |

- Set `Width` to the number of cells you give the input. The layout only learns the real width after the view runs, so the component cannot guess it.
- For a password field, set `Echo = TextInputComponent.Password '*'`. Use `Hidden` to show nothing at all.
- `CharLimit` caps the length. An emoji or a letter with its accent counts as one character.
- After a failed check, `model.Error` holds the message. Show it wherever you like.
- With suggestions, Tab accepts the one on screen. Your program decides what Tab means, so check `TextInputComponent.currentSuggestion` first, as the Components sample does.
- The cursor blinks only while the input has focus. Subscribe with `TextInputComponent.subscribe focused model |> Sub.map NameMsg`.
- Key bindings live in `model.Keymap`. Replace single bindings to remap them.

## ListComponent

A scrolling list with a marker on the focused row and a selection.

```fsharp
let fruits =
    ListComponent.init [ "apple"; "banana"; "cherry"; "damson" ] id 3
//                      items                          text  rows shown
```

| Message | What it does |
|---|---|
| `MoveUp`, `MoveDown` | move the marker |
| `Select` | select the marker row, or clear the selection if it is already selected |
| `KeyInput of KeyEvent` | Up, Down and Enter |

- The second argument turns an item into the text on screen. Use `id` for strings.
- Read the choice with `ListComponent.selectedItem model`, which returns an `option`.
- `ListComponent.withDirection ListComponent.BottomToTop` puts the first item at the bottom. Use it for chat-style lists.
- `Model.ViewportSize` is the number of rows shown. The list scrolls to keep the marker in view.

## TableComponent

Rows of styled cells under a header, with a cursor row. The table draws an outer border and a line under the header.

```fsharp
let columns: TableComponent.Column list =
    [ { Title = "Name"; Width = Fill 1 }
      { Title = "CPU"; Width = Len 5 } ]

let cells (p: Proc) = [ Ui.text p.Name; Ui.text $"{p.Cpu}%%" ]

//                          columns cells width height rows
let table, _ = TableComponent.init columns cells 80 20 processes
```

| Message | What it does |
|---|---|
| `Up`, `Down`, `PageUp`, `PageDown`, `Top`, `Bottom` | move the cursor |
| `KeyInput of KeyEvent` | arrow keys, Page Up/Down, Home, End |
| `MouseInput of MouseEvent` | the mouse wheel |

- Each column has a `Title` and a `Width`. A width is any layout constraint: `Len`, `Pct`, `Fill`, and so on.
- `RowToCells` returns one node per column. Use `Ui.text` or `Ui.line` to style parts of a cell.
- `Height` counts data rows. The header adds one more row.
- `Width = 0` lets each column take the size it asks for.
- The border is `Single` by default. Set `Border` (any `BorderStyle`, or `NoBorder`) and `BorderColor` to change it.
- `TableComponent.selectedRow model` returns the row under the cursor. `setRows` replaces the data and `setSize` handles a resize.

## ViewportComponent

A box of rich text that scrolls. It wraps long lines and can follow a growing log.

```fsharp
let help, _ =
    ViewportComponent.init 40 10 [ Ui.text "Keys" |> Ui.bold; Ui.text "Up and Down scroll." ]
//                         width height content
```

The content is a list of nodes, one for each paragraph, so every style works.

| Message | What it does |
|---|---|
| `LineUp`, `LineDown` | scroll one row |
| `PageUp`, `PageDown`, `HalfPageUp`, `HalfPageDown` | scroll by a page or half a page |
| `Top`, `Bottom` | jump to either end |
| `KeyInput of KeyEvent` | Up, Down, Page Up/Down, Space, Ctrl+U, Ctrl+D, Home, End |
| `MouseInput of MouseEvent` | the mouse wheel, `WheelStep` rows for each notch |

Things to know:

- `Width` and `Height` are the inside size of the box. If you put the viewport in a bordered box, subtract the border and padding. Call `setSize` when the terminal is resized.
- `setContent` and `setText` replace the content. `setText` makes one paragraph for each line and drops one trailing newline, so `"a\nb\n"` gives two paragraphs.
- A viewport at the bottom stays at the bottom, so a log follows new lines.
- Content that fits in the box counts as both the top and the bottom. New content, or a smaller size, then shows the end. To show a document from its start, load it through `init`, and send `Top` after `setContent`, `setText` or `setSize`.
- A paragraph's own size, border and padding are ignored. Its background colours only the text, not the whole row.
- Tabs and other control characters are dropped. Expand tabs before you pass the text in.
- `atTop`, `atBottom` and `scrollPercent` help you draw a scroll hint, for example in the box title.

## HelpComponent

A line of key hints, or a full grid, built from your `Keymap` bindings.

```fsharp
let bind keys key description =
    Keymap.setHelp
        (Keymap.KeyBind.create keys)
        (Some { Keymap.Key = key; Keymap.Description = description })

let quit = bind [ KeyEvent.plain Key.Esc ] "esc" "quit"
let move = bind [ KeyEvent.plain Key.Up; KeyEvent.plain Key.Down ] "↑/↓" "move"

HelpComponent.view [ move; quit ] [ [ move ]; [ quit ] ] model.Help
//                 short line     full grid (a list of columns)
```

- `ShowAll = false` draws the short line. `HelpComponent.toggle` flips between the line and the grid.
- Set `Width` so the line can cut itself off with `Ellipsis` when the hints do not fit. `0` means no limit.
- A disabled binding, or one without help text, does not show.
- Change the look with `KeyStyle`, `DescriptionStyle`, `SeparatorStyle` and `Separator`.

## CheckboxComponent

A toggle with a label.

```fsharp
let agree, _ = CheckboxComponent.init "I have read the checklist"

// update
| (Key.Enter | Key.Char ' '), Agree ->
    let agree, _ = CheckboxComponent.update CheckboxComponent.Toggle model.Agree
    { model with Agree = agree }, Cmd.none
```

Read the state from `model.Checked`. Change the marks with `CheckedChar` and `UncheckedChar`, and the look with `CheckedStyle` and `UncheckedStyle`.

## ButtonComponent

A label in a box. Its only message is `Pressed`.

```fsharp
let submit, _ = ButtonComponent.init "Submit"

ButtonComponent.view (Focus.isFocused Submit model.Focus) model.Submit
```

The component does not read keys. Your `update` decides which key presses the button, then acts. `Style` and `FocusedStyle` set the look.

## TimerComponent

A countdown. Create it with a tick interval and a start time.

```fsharp
let timer, _ = TimerComponent.init 100<ms> (System.TimeSpan.FromSeconds 30.0)

// subscribe
TimerComponent.subscribe model.Timer |> Sub.map TimerMsg
```

| Message | What it does |
|---|---|
| `TickMsg` | lowers the time left by one interval |
| `StartStopMsg` | pauses or resumes. Send it with the command `model.Toggle()` |
| `TimedOutMsg` | arrives once, when the time reaches zero |

The timer has its own id, so two timers never react to each other's messages. Match `TimedOutMsg` in your `update` to react when the time is up.

## SpinnerComponent

An animated spinner with a label.

```fsharp
let spinner, _ = SpinnerComponent.init SpinnerComponent.Dots 80<ms> "working"

// subscribe
SpinnerComponent.subscribe model.Spinner |> Sub.map SpinnerMsg
```

Styles are `Dots`, `Line`, `Braille` and `Custom [| "a"; "b" |]`. Show it only while work runs: subscribe when busy and stop when done.

## ProgressBarComponent

A bar filled in proportion to a value from `0.0` to `1.0`.

```fsharp
let progress = ProgressBarComponent.init 20            // 20 cells wide

let progress = ProgressBarComponent.setProgress 0.35 progress
```

Set `ShowPercentage = false` to hide the number. `FilledChar`, `EmptyChar`, `FilledStyle` and `EmptyStyle` change the look.
