namespace Fansi.Core

open System.Globalization
open System.Text

/// What fills one cell or two: a code point plus any marks that sit on it.
[<Struct>]
type Glyph = { Chars: string; Cells: int }

module Width =
    /// Code points terminals draw two cells wide: East Asian Wide and Fullwidth,
    /// and emoji drawn as pictures by default. Sorted, for the binary search below.
    let internal wideRanges: (int * int) array =
        [| 0x1100, 0x115F
           0x231A, 0x231B
           0x2329, 0x232A
           0x23E9, 0x23EC
           0x23F0, 0x23F0
           0x23F3, 0x23F3
           0x25FD, 0x25FE
           0x2614, 0x2615
           0x2648, 0x2653
           0x267F, 0x267F
           0x2693, 0x2693
           0x26A1, 0x26A1
           0x26AA, 0x26AB
           0x26BD, 0x26BE
           0x26C4, 0x26C5
           0x26CE, 0x26CE
           0x26D4, 0x26D4
           0x26EA, 0x26EA
           0x26F2, 0x26F3
           0x26F5, 0x26F5
           0x26FA, 0x26FA
           0x26FD, 0x26FD
           0x2705, 0x2705
           0x270A, 0x270B
           0x2728, 0x2728
           0x274C, 0x274C
           0x274E, 0x274E
           0x2753, 0x2755
           0x2757, 0x2757
           0x2795, 0x2797
           0x27B0, 0x27B0
           0x27BF, 0x27BF
           0x2B1B, 0x2B1C
           0x2B50, 0x2B50
           0x2B55, 0x2B55
           0x2E80, 0x303E
           0x3041, 0x33FF
           0x3400, 0x4DBF
           0x4E00, 0x9FFF
           0xA000, 0xA4CF
           0xA960, 0xA97F
           0xAC00, 0xD7A3
           0xF900, 0xFAFF
           0xFE10, 0xFE19
           0xFE30, 0xFE6F
           0xFF00, 0xFF60
           0xFFE0, 0xFFE6
           0x16FE0, 0x16FE4
           0x17000, 0x18CFF
           0x1B000, 0x1B2FF
           0x1F004, 0x1F004
           0x1F0CF, 0x1F0CF
           0x1F18E, 0x1F18E
           0x1F191, 0x1F19A
           0x1F200, 0x1F202
           0x1F210, 0x1F23B
           0x1F240, 0x1F248
           0x1F250, 0x1F251
           0x1F260, 0x1F265
           0x1F300, 0x1F64F
           0x1F680, 0x1F6FF
           0x1F7E0, 0x1F7EB
           0x1F90C, 0x1F9FF
           0x1FA70, 0x1FAFF
           0x20000, 0x2FFFD
           0x30000, 0x3FFFD |]

    let private isWide (codePoint: int) =
        let mutable lo = 0
        let mutable hi = wideRanges.Length - 1
        let mutable found = false

        while not found && lo <= hi do
            let mid = (lo + hi) / 2
            let first, last = wideRanges[mid]

            if codePoint < first then hi <- mid - 1
            elif codePoint > last then lo <- mid + 1
            else found <- true

        found

    let ofRune (r: Rune) =
        if r.Value = 0xAD then
            1 // terminals draw a soft hyphen as a visible hyphen
        else
            match Rune.GetUnicodeCategory r with
            | UnicodeCategory.Control
            | UnicodeCategory.NonSpacingMark
            | UnicodeCategory.EnclosingMark
            | UnicodeCategory.Format -> 0
            | _ -> if isWide r.Value then 2 else 1

    let isBidiControl (r: Rune) =
        (r.Value >= 0x202A && r.Value <= 0x202E)
        || (r.Value >= 0x2066 && r.Value <= 0x2069)

    /// Split text into glyphs. A control character would drive the terminal instead
    /// of drawing, so it is dropped, and so is a mark with nothing to sit on.
    /// EnumerateRunes already turns a lone surrogate into U+FFFD.
    let glyphs (text: string) : Glyph list =
        if isNull text then
            []
        else
            let result = ResizeArray<Glyph>()

            for r in text.EnumerateRunes() do
                // Bidi controls reorder the text the user sees, e.g. to disguise a file name.
                if Rune.GetUnicodeCategory r <> UnicodeCategory.Control && not (isBidiControl r) then
                    let cells = ofRune r

                    if cells > 0 then
                        result.Add { Chars = r.ToString(); Cells = cells }
                    elif result.Count > 0 then
                        let last = result[result.Count - 1]

                        result[result.Count - 1] <-
                            { last with
                                Chars = last.Chars + r.ToString() }

            List.ofSeq result

    let ofString (text: string) =
        glyphs text |> List.sumBy (fun g -> g.Cells)
