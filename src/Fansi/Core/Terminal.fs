namespace Fansi.Core

open System
open System.Reflection
open System.Runtime.InteropServices
open System.Threading

/// Raw mode outlives the process. A shell left with echo and line editing off is
/// unusable until the user types `reset` blind, so every path out of here puts the
/// terminal back.
///
/// Raw mode also takes Ctrl+C away from the driver, so Ctrl+C no longer kills the
/// program. The exit hooks below cover signals from elsewhere and Environment.Exit;
/// Ctrl+C is not an escape hatch any more.
module Terminal =

    module Windows =
        [<Literal>]
        let ENABLE_PROCESSED_INPUT = 0x0001u

        [<Literal>]
        let ENABLE_LINE_INPUT = 0x0002u

        [<Literal>]
        let ENABLE_ECHO_INPUT = 0x0004u

        [<Literal>]
        let ENABLE_VIRTUAL_TERMINAL_INPUT = 0x0200u

        [<Literal>]
        let ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004u

        let rawInputMode (mode: uint32) =
            (mode &&& ~~~(ENABLE_LINE_INPUT ||| ENABLE_ECHO_INPUT ||| ENABLE_PROCESSED_INPUT))
            ||| ENABLE_VIRTUAL_TERMINAL_INPUT

        /// SetConsoleMode is all or nothing. A conhost older than Windows 10 1809
        /// rejects ENABLE_VIRTUAL_TERMINAL_INPUT and then applies none of the other
        /// flags, leaving echo on, so the retry drops it and keeps the rest.
        let withoutVirtualTerminalInput (mode: uint32) =
            mode &&& ~~~ENABLE_VIRTUAL_TERMINAL_INPUT

        /// Renderer.fs writes escape sequences directly and nothing else turns this
        /// on. Windows Terminal defaults to it; an older conhost prints the escapes
        /// as text without it.
        let vtOutputMode (mode: uint32) =
            mode ||| ENABLE_VIRTUAL_TERMINAL_PROCESSING

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern nativeint private GetStdHandle(int nStdHandle)

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern bool private GetConsoleMode(nativeint hConsoleHandle, uint32& lpMode)

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern bool private SetConsoleMode(nativeint hConsoleHandle, uint32 dwMode)

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern uint32 private GetConsoleCP()

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern bool private SetConsoleCP(uint32 wCodePageID)

        let internal stdIn () = GetStdHandle(-10)
        let internal stdOut () = GetStdHandle(-11)

        let internal tryGetMode (handle: nativeint) =
            let mutable mode = 0u
            if GetConsoleMode(handle, &mode) then Some mode else None

        let internal setMode (handle: nativeint) (mode: uint32) = SetConsoleMode(handle, mode)

        [<Literal>]
        let Utf8CodePage = 65001u

        /// Zero means the call failed.
        let internal inputCodePage () = GetConsoleCP()

        let internal setInputCodePage (codePage: uint32) = SetConsoleCP codePage

    module Unix =

        /// The flag values raw mode clears. They differ between Linux and macOS.
        type Flags =
            { Icanon: uint32
              Echo: uint32
              Isig: uint32
              Iexten: uint32
              Ixon: uint32 }

        /// Where c_iflag and c_lflag sit inside termios, and how wide each field is.
        /// Only these two fields are ever touched, so the whole struct never has to
        /// be declared twice.
        type Layout =
            { IflagOffset: int
              LflagOffset: int
              WordSize: int }

        let flagsFor (isMacOs: bool) =
            if isMacOs then
                { Icanon = 0x00000100u
                  Echo = 0x00000008u
                  Isig = 0x00000080u
                  Iexten = 0x00000400u
                  Ixon = 0x00000200u }
            else
                { Icanon = 0x0002u
                  Echo = 0x0008u
                  Isig = 0x0001u
                  Iexten = 0x8000u
                  Ixon = 0x0400u }

        let layoutFor (isMacOs: bool) =
            if isMacOs then
                { IflagOffset = 0
                  LflagOffset = 24
                  WordSize = 8 }
            else
                { IflagOffset = 0
                  LflagOffset = 12
                  WordSize = 4 }

        /// IEXTEN has to go with the other three: with it on, the driver still eats
        /// Ctrl+V as `lnext` and, on macOS, Ctrl+O and Ctrl+Y.
        let rawLocalFlags (f: Flags) (lflag: uint32) =
            lflag &&& ~~~(f.Icanon ||| f.Echo ||| f.Isig ||| f.Iexten)

        let rawInputFlags (f: Flags) (iflag: uint32) = iflag &&& ~~~f.Ixon

        /// Large enough for termios on either platform.
        [<Literal>]
        let internal BufferSize = 128

        let internal readWord (buffer: byte[]) (offset: int) (wordSize: int) : uint64 =
            if wordSize = 4 then
                uint64 (BitConverter.ToUInt32(buffer, offset))
            else
                BitConverter.ToUInt64(buffer, offset)

        let internal writeWord (buffer: byte[]) (offset: int) (wordSize: int) (value: uint64) =
            let bytes =
                if wordSize = 4 then
                    BitConverter.GetBytes(uint32 value)
                else
                    BitConverter.GetBytes value

            Array.blit bytes 0 buffer offset wordSize

        /// The flag values all fit in 32 bits, but c_lflag is 8 bytes wide on macOS.
        /// Keep the upper half as it was read so a flag that lands up there one day
        /// survives the round trip.
        let internal applyToWord (transform: uint32 -> uint32) (word: uint64) =
            (word &&& 0xFFFFFFFF00000000UL) ||| uint64 (transform (uint32 word))

        /// Edits the buffer in place. This is the one place a swap would put the
        /// input flags into c_lflag, so it is a function the tests can reach.
        let internal applyRawFlags (f: Flags) (layout: Layout) (buffer: byte[]) =
            let lflag = readWord buffer layout.LflagOffset layout.WordSize
            let iflag = readWord buffer layout.IflagOffset layout.WordSize

            writeWord buffer layout.LflagOffset layout.WordSize (applyToWord (rawLocalFlags f) lflag)
            writeWord buffer layout.IflagOffset layout.WordSize (applyToWord (rawInputFlags f) iflag)

        /// tcsetattr from a background process group raises SIGTTOU, which stops the
        /// process. A job started with `&` still has the tty on stdin and looks
        /// interactive, so raw mode is only entered when we own the terminal.
        /// tcgetpgrp returns -1 when stdin is not a terminal at all.
        let internal isForeground (terminalGroup: int) (ownGroup: int) =
            terminalGroup >= 0 && terminalGroup = ownGroup

        [<DllImport("libc")>]
        extern int private tcgetattr(int fd, byte[] termios)

        [<DllImport("libc")>]
        extern int private tcsetattr(int fd, int optionalActions, byte[] termios)

        [<DllImport("libc")>]
        extern int private tcgetpgrp(int fd)

        [<DllImport("libc")>]
        extern int private getpgrp()

        /// The runtime probes libc, libc.so, liblibc and liblibc.so for the name
        /// "libc". glibc ships libc.so.6 plus a libc.so that is a linker script
        /// dlopen refuses, and musl ships neither, so the plain name fails on Linux.
        /// macOS resolves it from the dyld cache, so it falls through to the default.
        let private libcResolver =
            DllImportResolver(fun name _ _ ->
                if name = "libc" then
                    [ "libc.so.6"; "libc.musl-x86_64.so.1"; "libc.musl-aarch64.so.1" ]
                    |> List.tryPick (fun candidate ->
                        match NativeLibrary.TryLoad candidate with
                        | true, handle -> Some handle
                        | _ -> None)
                    |> Option.defaultValue IntPtr.Zero
                else
                    IntPtr.Zero)

        let private resolverInstalled =
            lazy
                (try
                    NativeLibrary.SetDllImportResolver(Assembly.GetExecutingAssembly(), libcResolver)
                 with :? InvalidOperationException ->
                     ())

        let internal ensureLibcResolver () = resolverInstalled.Force()

        let internal ownsTerminal () = isForeground (tcgetpgrp 0) (getpgrp ())

        let internal getAttr (buffer: byte[]) = tcgetattr (0, buffer) = 0

        /// TCSANOW is 0 on both platforms.
        let internal setAttr (buffer: byte[]) = tcsetattr (0, 0, buffer) = 0

    let mutable private raw = false

    /// Whether raw mode is actually on. False when there is no console and when the
    /// driver rejected the change, and it stays true if a restore is refused.
    let isRaw () = raw

    /// Everything that can fail is split in two. Restore is registered before Apply
    /// runs, so a throw part-way through entering can never leave the terminal raw
    /// with nothing to put it back. Both report whether the driver took the change.
    type private Entry =
        { Restore: unit -> bool
          Apply: unit -> bool }

    let private prepareWindows () =
        let input = Windows.stdIn ()
        let output = Windows.stdOut ()

        match Windows.tryGetMode input with
        | None -> None
        | Some originalIn ->
            let originalOut = Windows.tryGetMode output
            let originalCodePage = Windows.inputCodePage ()

            // the input handle carries echo and line input, so it is the one whose
            // restore decides whether the terminal is back
            let restore () =
                let back = Windows.setMode input originalIn

                match originalOut with
                | Some mode -> Windows.setMode output mode |> ignore
                | None -> ()

                if originalCodePage <> 0u then
                    Windows.setInputCodePage originalCodePage |> ignore

                back

            let apply () =
                let wanted = Windows.rawInputMode originalIn

                let entered =
                    Windows.setMode input wanted
                    || Windows.setMode input (Windows.withoutVirtualTerminalInput wanted)

                // VT output is a nicety, and there is no point touching it when raw
                // mode did not engage
                match entered, originalOut with
                | true, Some mode -> Windows.setMode output (Windows.vtOutputMode mode) |> ignore
                | _ -> ()

                // The input stream reads bytes in the console's input code page, which
                // is OEM 850 or 437 by default. The parser expects UTF-8, so without
                // this every accented letter arrives as a stray byte and is dropped.
                if entered then
                    Windows.setInputCodePage Windows.Utf8CodePage |> ignore

                entered

            Some { Restore = restore; Apply = apply }

    let private prepareUnix () =
        Unix.ensureLibcResolver ()
        let original = Array.zeroCreate<byte> Unix.BufferSize

        if not (Unix.ownsTerminal ()) then
            None
        else
            // .NET sets up its console on the first console call. It then snapshots
            // termios and writes that snapshot back at exit. Left to happen on the
            // first frame, after raw mode is on, it would snapshot raw mode and put
            // it back after we restore, leaving the shell without echo. Triggering it
            // here makes it snapshot cooked mode instead.
            Console.TreatControlCAsInput |> ignore

            if not (Unix.getAttr original) then
                None
            else
                let isMacOs = RuntimeInformation.IsOSPlatform OSPlatform.OSX
                let working = Array.copy original
                Unix.applyRawFlags (Unix.flagsFor isMacOs) (Unix.layoutFor isMacOs) working

                Some
                    { Restore = fun () -> Unix.setAttr original
                      Apply = fun () -> Unix.setAttr working }

    let private tryPrepare () =
        if raw || Console.IsInputRedirected then
            None
        else
            try
                if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                    prepareWindows ()
                else
                    prepareUnix ()
            with _ ->
                None

    /// The keyboard cannot send these while raw mode is on, but another process can.
    /// Their default action ends the process without running ProcessExit.
    let private signalsFromElsewhere = [ PosixSignal.SIGINT; PosixSignal.SIGQUIT ]

    /// Put the terminal into raw mode, and run `leave` once on the way out, just
    /// before the terminal modes go back. Disposing the result does both.
    ///
    /// Both are also hooked to process exit, to an unhandled exception, and to
    /// SIGINT and SIGQUIT sent from elsewhere, so a plain `kill` restores as much as
    /// a normal exit does. `leave` is for what the caller switched on itself, such as
    /// the alternate screen or mouse reporting. It runs even when raw mode could not
    /// be entered, because the caller's own modes are on either way.
    let enterRawModeWith (leave: unit -> unit) : IDisposable =
        let prepared = tryPrepare ()

        // ProcessExit, UnhandledException and the signals fire on other threads, so
        // two callers can reach these at once
        let restored = ref 0
        let left = ref 0

        // a restore the driver refuses leaves the terminal raw, so the flag has to
        // stay true for it
        let restoreModes () =
            match prepared with
            | Some entry when Interlocked.Exchange(&restored.contents, 1) = 0 -> raw <- not (entry.Restore())
            | _ -> ()

        let leaveOnce () =
            if Interlocked.Exchange(&left.contents, 1) = 0 then
                // Leave first: on Windows the restore can turn VT output off again,
                // and the caller's escapes would then print as text.
                try
                    leave ()
                finally
                    restoreModes ()

        // a hook must never throw: it runs inside the runtime's exit or signal path
        let quietly () =
            try
                leaveOnce ()
            with _ ->
                ()

        let onExit = EventHandler(fun _ _ -> quietly ())
        let onCrash = UnhandledExceptionEventHandler(fun _ _ -> quietly ())

        AppDomain.CurrentDomain.ProcessExit.AddHandler onExit
        AppDomain.CurrentDomain.UnhandledException.AddHandler onCrash

        // The handler does not cancel the signal, so its default action still ends
        // the process once the terminal is back.
        let signals =
            signalsFromElsewhere
            |> List.choose (fun signal ->
                try
                    Some(PosixSignalRegistration.Create(signal, (fun _ -> quietly ())))
                with _ ->
                    None)

        // a long-running program that enters and leaves raw mode repeatedly would
        // otherwise pile up a set of handlers per call
        let unhook () =
            AppDomain.CurrentDomain.ProcessExit.RemoveHandler onExit
            AppDomain.CurrentDomain.UnhandledException.RemoveHandler onCrash
            signals |> List.iter (fun registration -> registration.Dispose())

        match prepared with
        | None -> ()
        | Some entry ->
            let entered =
                try
                    entry.Apply()
                with _ ->
                    false

            if entered then
                raw <- true
            else
                // the restore here undoes at most a part-applied entry, so its result
                // must not be allowed to mark the terminal raw
                try
                    restoreModes ()
                with _ ->
                    ()

                raw <- false

        { new IDisposable with
            member _.Dispose() =
                try
                    leaveOnce ()
                finally
                    unhook () }

    /// Put the terminal into raw mode. Disposing the result restores it.
    let enterRawMode () : IDisposable = enterRawModeWith ignore
