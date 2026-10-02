namespace Fansi.Core

[<RequireQualifiedAccess>]
type Color =
    | Default
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | BrightBlack
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite
    | Rgb of r: int * g: int * b: int

type Style =
    { FgColor: Color
      BgColor: Color
      Bold: bool
      Italic: bool
      Underline: bool
      Strikethrough: bool }

    static member Default =
        { FgColor = Color.Default
          BgColor = Color.Default
          Bold = false
          Italic = false
          Underline = false
          Strikethrough = false }

/// A piece of text in one style. A line is a list of runs.
type Run = { Text: string; Style: Style }
