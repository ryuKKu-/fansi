# Architecture

This document explains how Fansi is built: how a model becomes a screen, how a key becomes a message, and how the
parts in `src/Fansi` fit together.

For the ready-made components, see the [component guide](components.md).

## The big picture

A Fansi app is an [Elmish](https://elmish.github.io/elmish/) program. You give it three functions: `init`, `update`
and `view`. Fansi adds the terminal around them: it reads input, turns it into messages, and draws each new view.

```
                      ┌─────────────────────────────────────────────┐
                      │                  Your app                   │
                      │                                             │
                      │   init ──► Model ──► view ──► Node tree     │
                      │              ▲                    │         │
                      │              │                    │         │
                      │           update ◄── FansiMsg     │         │
                      └──────────────────────▲────────────┼─────────┘
                                             │            │
   keyboard, mouse,   ┌──────────────┐  ┌────┴─────┐  ┌───▼──────┐  ┌──────────┐
   paste, resize ───► │   Terminal   ├─►│ Program  │  │  Layout  ├─►│ Painting │
                      │ InputParser  │  │   loop   │  │  Solver  │  │  Buffer  │
                      └──────────────┘  └──────────┘  └──────────┘  └────┬─────┘
                                                                         │
                                                                    ┌────▼─────┐
                                                        screen ◄────┤ Renderer │
                                                                    └──────────┘
```

## Where the code lives

The library is in `src/Fansi`. F# compiles files in order, so each folder only uses the folders above it.

| Folder | What it holds |
|---|---|
| `Core` | geometry (`Rect`, `Edges`), `Style`, `Props` and the `Node` tree |
| `Text` | display width of text, and styled runs of text with wrapping |
| `Ui.fs` | the functions that build a view |
| `Layout` | the constraint solver and the layout pass |
| `Rendering` | the cell buffer, painting, and the renderer that writes to the terminal |
| `Terminal` | input events, the input parser, and raw mode |
| `Program` | the Elm loop, `Cmd` and `Sub` helpers, keymaps, focus and the text cursor |
| `Components` | ready-made components, built only with the public API |

## Lifecycle of a program

`FansiProgram.run` prepares the terminal, runs the loop until something asks to quit, and then puts the terminal
back as it was.

```
 FansiProgram.run
   │
   ├─ enter raw mode           no echo, no line editing, exit hooks installed
   ├─ switch on                alternate screen, hidden cursor, bracketed paste,
   │                           focus reporting, mouse tracking (if enabled)
   ├─ start the renderer       a timer that flushes frames, 60 per second by default
   │
   ├─ init                     first model and first commands
   ├─ first paint
   ├─ watch for resize         SIGWINCH, or polling every 200 ms on Windows
   ├─ start the input reader   its own background thread
   │
   │      ... messages flow until Cmd.quit, Ctrl+C or end of input ...
   │
   ├─ stop the renderer        paints the last pending frame
   └─ switch everything off    then leave raw mode
```

Raw mode outlives the process. A shell left in raw mode is unusable, so the teardown also runs on a crash, on
`ProcessExit` and on signals such as `SIGQUIT`. If `update` throws, the program stops and `run` throws the same
exception on the caller's thread.

## The program loop

### Messages

`update` receives a `FansiMsg<'msg>` and returns a plain `Cmd<'msg>`:

| Case | When |
|---|---|
| `KeyPress of KeyEvent` | a key, with `Ctrl`, `Alt` and `Shift` flags |
| `Mouse of MouseEvent` | a click, a scroll or a move, once `withMouseEnabled` is on |
| `Paste of string` | a whole bracketed paste, as one message |
| `Resize of width * height` | the terminal changed size |
| `FocusChanged of bool` | the terminal window gained or lost focus |
| `App of 'msg` | your own messages, from commands and subscriptions |

Fansi wraps the commands and subscriptions you return in `App`. So a child component only needs `Cmd.map` and
`Sub.map`, never anything Fansi-specific.

### One message at a time

Messages come from several threads: the input reader, the resize watcher, and every timer. They all go through one
`dispatch` function, which puts them in a queue. Only one thread at a time handles the queue.

```
  input reader ──┐
  resize watch ──┤
  Sub.timer    ──┼──► dispatch ──► queue ──► pump (one thread at a time, under a lock)
  Cmd.after    ──┘                              │
                                                ▼
                        ┌───────────────────────────────────────────────┐
                        │ for each message in the queue:                │
                        │                                               │
                        │   1. model', cmd = update msg model           │
                        │   2. if model' is a new object, or the        │
                        │      terminal was resized:                    │
                        │         view model' ──► paint ──► new frame   │
                        │   3. run cmd (it may dispatch more messages,  │
                        │      which join the queue)                    │
                        │   4. start or stop subscriptions that         │
                        │      changed since the last message           │
                        │   5. stop here if Cmd.quit was called         │
                        └───────────────────────────────────────────────┘
```

Two details matter here:

- The loop compares models by reference, not by value. If `update` returns the same model object, `view` does not
  run. Returning `model, Cmd.none` for a message you ignore is free.
- A message dispatched while the pump is busy, for example by a command, is queued and handled in the same pass. It
  never runs `update` inside another `update`.

### Commands

- `Cmd.quit` stops the program after the current message.
- `Cmd.after 500<ms> msg` sends a message once, after a delay.
- Ctrl+C quits unless you use `withoutQuitOnCtrlC`. The app still receives the key in both cases.

### Subscriptions

After each message, Fansi calls your `subscribe` function and compares the result with the running subscriptions, by
id. New ids start, missing ids stop, and the others keep running.

Components that tick, such as the timer, the spinner and the text cursor, get a unique id when they are created. Their
subscription ids include it, so two instances never collide and `Sub.map` needs no key:

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

For a timer of your own, use `Sub.timer [ "clock" ] 1000<ms> Tick`.

## The rendering pipeline

Each time `view` runs, the result goes through four steps:

```
   view model
       │
       ▼
  ┌──────────┐   Node tree: Text, Line and Container nodes,
  │   Node   │   each with a Style and Props (size, border, padding...)
  └────┬─────┘
       │  Layout.arrange
       ▼
  ┌──────────┐   LayoutNode tree: the same nodes, each with a Rect
  │  Layout  │   (where it goes) and a Clip (where it may draw)
  └────┬─────┘
       │  Paint.node
       ▼
  ┌──────────┐   Buffer: a grid of cells, width × height.
  │  Buffer  │   Each cell holds one symbol and its style
  └────┬─────┘
       │  Renderer.SetFrame            (on the pump thread)
       ▼
  ┌──────────┐   On each tick: compare with the last frame and
  │ Renderer │   write only the cells that changed  (on the timer thread)
  └──────────┘
```

Layout and painting are pure: they take a tree and return a new buffer. Only the renderer talks to the terminal.
This is why the tests can check layout and painting without a real terminal.

### Views are data

`view` returns a `Node`. Constructors build one, and every modifier is a `Node -> Node` function that changes its
`Style` or `Props`. So you can restyle a node that a component already built:

```fsharp
TimerComponent.view model.Timer |> Ui.border Single |> Ui.fill 1
```

| Group | Functions |
|---|---|
| Constructors | `Ui.text`, `Ui.line`, `Ui.row`, `Ui.col`, `Ui.empty` |
| Size along the parent axis | `Ui.len`, `Ui.pct`, `Ui.ratio`, `Ui.fill`, `Ui.minLen`, `Ui.maxLen`, `Ui.auto` |
| Box | `Ui.border`, `Ui.title`, `Ui.titleWith`, `Ui.pad`, `Ui.padX`, `Ui.padY`, `Ui.margin`, `Ui.justify`, `Ui.align` |
| Style | `Ui.fg`, `Ui.bg`, `Ui.bold`, `Ui.italic`, `Ui.underline`, `Ui.strike`, `Ui.style` |

There are three kinds of node:

- `Text` draws a string, wrapped to its width.
- `Container` (from `Ui.row` and `Ui.col`) places its children side by side or one under the other.
- `Line` draws its children as one run of text, each part in its own style. Inside a line, only text and style
  count. A row or column inside a line gives its text, and its border and padding are ignored.

```fsharp
Ui.line [ Ui.text "Status: "; Ui.text "ok" |> Ui.bold |> Ui.fg Color.Green ]
```

### The box of a node

Every node has the same box, from the outside in:

```
  ┌ slot given by the parent ─────────────────────┐
  │                  margin                       │
  │   ╭─ border (1 cell, or 0 with NoBorder) ─╮   │
  │   │              padding                  │   │
  │   │   ┌─ content ──────────────────────┐  │   │
  │   │   │ text, or the children          │  │   │
  │   │   └────────────────────────────────┘  │   │
  │   ╰───────────────────────────────────────╯   │
  └───────────────────────────────────────────────┘
```

The border and background fill the box inside the margin. Text and children go in the content area.

### Layout

Layout runs in two passes over the tree.

1. **Measure** (`Layout.measure`). Going down, each node learns how much room it could get. Going back up, it reports
   the size it wants: the width of its longest text row and its number of rows, or the sum of its children, plus its
   margin, border and padding.
2. **Place** (`Layout.arrange`). Going down again, each container splits its content area between its children and
   gives each child a slot. The child takes off its margin, border and padding, and does the same for its own
   children.

A `Ui.row` splits its width between its children. A `Ui.col` splits its height. That direction is the *main axis*.
The other one is the *cross axis*.

- On the main axis, the solver decides each child's size, and `Ui.justify` decides where the leftover space goes.
- On the cross axis, `Ui.align` decides. The default, `Stretch`, gives the child the whole height of a row or width
  of a column.

Each node also gets a clip rectangle: its own box, cut to its parent's content area. Painting never writes outside the
clip, so a child that is too big is cut off and never draws over its parent's border.

### The solver

`Solver.solve` sizes the children of one container along its main axis. It works in whole cells.

```
 1. Base size for each child
      Len n       → n
      Pct p       → p% of the space
      Ratio(a, b) → a/b of the space
      Auto        → its measured size
      Max n       → its measured size, at most n
      Min n       → n, and it can grow
      Fill w      → 0, and it can grow with weight w

 2a. Bases fit?  Share the rest between the children that can grow,
                 in proportion to their weight (Min counts as weight 1).

 2b. Too big?    Shrink from the last child to the first, until it fits.
```

When at least one child can grow, the sizes always add up to the size of the parent. Integer division leaves a few
cells over. Those go to the children with the largest fractional part, so no cell is lost to rounding:

```
  width 10, three children with Ui.fill 1

  exact shares:  3.33   3.33   3.33      → whole cells: 3 + 3 + 3 = 9
  one cell left, the tie goes to the first child

  ┌──────────┬─────────┬─────────┐
  │  4 cells │ 3 cells │ 3 cells │       = 10, no gap
  └──────────┴─────────┴─────────┘
```

```fsharp
Ui.row
    [ Ui.text "sidebar" |> Ui.len 20     // exactly 20 cells
      Ui.text "main" |> Ui.fill 1 ]     // the rest
```

### Text width

Fansi measures text in terminal cells, not characters. CJK characters and emoji take two cells. `Text/Width.fs` splits
a string into glyphs and gives each one a width. Layout, wrapping, painting and cursor movement all use this width.

Wrapping is by cell, with no word awareness. A wide character never splits in half: if it does not fit at the end of
a row, it moves to the next row.

### Painting

`Paint.node` walks the layout tree from the root down. For each node:

1. If its style has a background, fill its box with it.
2. Draw its border, and the title inside the top border.
3. For text and lines, wrap the runs to the content width and write them row by row.
4. For containers, paint the children in order. Later siblings are drawn on top.

Styles pass from parent to child. A child inherits the foreground colour unless it sets its own. Bold, italic,
underline and strikethrough add up: a child of a bold node is always bold. Text with no background keeps the
background that is already in the cell, so a container's background shows through its children.

A wide character takes two cells. The first holds the symbol, and the second is marked as a *continuation*. Writing
over either half blanks the other half, so the terminal never gets half a glyph.

### The renderer

The renderer keeps two buffers: the one on screen and the newest one. `SetFrame` replaces the newest one and marks it
dirty. It does not write anything. A timer calls `Flush` at the frame rate, so ten model changes between two ticks
cost one write.

```
  previous (on screen)        current (new frame)         written to terminal
  ┌───────────────────┐       ┌───────────────────┐
  │ count: 41         │       │ count: 42         │       ESC[1;9H  "2"
  │ up / down ...     │       │ up / down ...     │       (move, then one cell)
  └───────────────────┘       └───────────────────┘
```

`Flush` goes through the cells row by row:

- An unchanged cell is skipped.
- A run of changed cells is written in one go, after a single cursor move.
- A style escape sequence is written only when the style changes from the previous cell.
- If the terminal size changed, the new frame is compared with a blank buffer, and the screen is erased in the same
  write as the redraw. The screen never shows a blank frame.

## Input

The input reader runs on its own background thread. It reads raw bytes from stdin and turns them into events.

```
  stdin bytes
      │
      ▼
  ┌─────────────────────────────────────────────┐
  │ pending = bytes left over + new bytes       │
  └───────────────┬─────────────────────────────┘
                  │  InputParser.parse
                  ▼
  ┌─────────────────────────────────────────────┐
  │ complete sequences ──► InputEvent list      │──► FansiMsg ──► dispatch
  │ unfinished tail    ──► kept for next read   │
  └─────────────────────────────────────────────┘
```

A terminal sends most keys as escape sequences. For example, Up is `ESC [ A`. This brings two problems:

- **A sequence can arrive in pieces.** The parser only takes complete sequences. It keeps the unfinished tail and
  waits for the next read.
- **Esc alone and Esc as the start of a sequence look the same.** When the tail starts with `ESC` and nothing else
  comes within the escape timeout (50 ms by default), `InputParser.parseFinal` reads it as the Esc key. Over a slow
  link, increase the timeout with `withEscapeTimeout`.

A bracketed paste arrives between two markers, `ESC [200~` and `ESC [201~`. The reader collects everything between
them and sends it as one `Paste` message, even when it spans many reads. If the end marker never comes, the reader
sends what it has after two seconds of silence.

Raw mode turns Ctrl+C into a plain byte, so the terminal does not stop the program. The reader sees the key, sends
it to `update`, and then stops the program, unless the app used `withoutQuitOnCtrlC`.

### Key bindings

`Keymap` describes a key binding with the keys, an optional help text, and an on/off switch. Use
``Keymap.``match`` binding keyEvent`` in `update`, and give the same bindings to `HelpComponent` so the help line
never drifts from the code.

## Components

A component is a small Elm program of its own: a module with a `Model`, a `Message` type, and `init`, `update`,
`view` and sometimes `subscribe`. It uses only the public API, so you can write your own in the same way.

```
  parent model ─── Timer: TimerComponent.Model
                     │
  parent update ──── TimerComponent.update msg model.Timer
                     │     returns (model, cmd) ──► Cmd.map TimerMsg
                     │
  parent view ────── TimerComponent.view model.Timer
                     │     returns a Node, restyle it with Ui.* if you like
                     │
  parent subscribe ─ TimerComponent.subscribe model.Timer ──► Sub.map TimerMsg
```

Each instance gets a unique id from `ComponentId.next` when it is created. Its messages carry that id, and `update`
ignores messages for other ids. So two timers can share one message type without mixing up their ticks.

### Focus

Components do not know whether they have focus. The parent keeps a `Focus` ring and passes a `bool` to each
component. One place in the app decides who has focus.

```fsharp
// model
Focus = Focus.ofList [ Name; Password; Submit ]

// update
| KeyPress k when k.Key = Key.Tab -> { model with Focus = Focus.next model.Focus }, Cmd.none

// view
TextInputComponent.view (Focus.isFocused Name model.Focus) model.Name
```

Use `Focus.next`, `Focus.prev` and `Focus.current` to move around and to find who has focus.

## Tests

`tests/Fansi.Tests` has one folder for each library folder, plus `Samples` for the sample apps. Layout, painting and
the parser work on plain data, so they are tested without a real terminal. A layout test builds a `Node`, calls
`Paint.render width height`, and checks the text of the resulting buffer with `Buffer.toLines`.
