namespace Fansi.Core

/// How a node claims space along one axis of its parent.
type Constraint =
    | Auto
    | Len of int
    | Pct of int
    | Ratio of int * int
    | Fill of int
    | Min of int
    | Max of int

type Direction =
    | Row
    | Column

[<RequireQualifiedAccess>]
type Justify =
    | Start
    | End
    | Center
    | Between

[<RequireQualifiedAccess>]
type Align =
    | Start
    | End
    | Center
    | Stretch

type BorderStyle =
    | NoBorder
    | Single
    | Double
    | Rounded
    | Heavy
    | Ascii

type Props =
    { Main: Constraint
      Cross: Constraint
      Direction: Direction
      Justify: Justify
      Align: Align
      Padding: Edges
      Margin: Edges
      Border: BorderStyle }

    static member Default =
        { Main = Auto
          Cross = Auto
          Direction = Column
          Justify = Justify.Start
          Align = Align.Stretch
          Padding = Edges.Zero
          Margin = Edges.Zero
          Border = NoBorder }
