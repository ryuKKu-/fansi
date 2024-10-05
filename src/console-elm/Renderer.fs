namespace Fansi

[<Measure>] type FPS

module internal Renderer =
    open System.Diagnostics
    open System.Timers
    open System

    type internal Renderer(fps: int<FPS>) =
        let [<Literal>] defaultFPS = 60
        let fpsToRender =
            let fps = int fps 
            if fps < 1 then defaultFPS
            else fps
        
        let frameRate = TimeSpan.FromSeconds(1) / TimeSpan(int64(fpsToRender * 1000))
        let ticker = new Timer(frameRate)
        let sw = Stopwatch()
        let mutable buffer = Unchecked.defaultof<_>
        let mutable previous = Unchecked.defaultof<_>
        
        let clear = "\x1b[2J\x1b[3J\x1b[1;1H"
        
        member this.Flush() =
            lock this (fun () -> 
                if String.IsNullOrEmpty buffer || buffer = previous then ()
                else
                    Console.Out.Write(clear)
                    Console.Out.Flush()
                    
                    Console.WriteLine(buffer)
                    previous <- buffer
            )
            
        member this.Start() =       
            if not ticker.Enabled then
                ticker.Elapsed.Add (fun _ -> this.Flush())
                ticker.Start()
                sw.Start()
        
        member this.Write(str) =
            lock this (fun () -> 
                if String.IsNullOrEmpty str then ()
                else buffer <- str
            )
            
        member this.Stop() =
            if ticker.Enabled then
                this.Flush()
                ticker.Stop()