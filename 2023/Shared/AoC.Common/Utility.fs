module AoC.Common.Utility

open System
open System.Diagnostics

type public ITimeTracker = 
    inherit IDisposable
    abstract Elapsed: TimeSpan
    abstract ElapsedString: string

let public GetTracker () =
    let sw = Stopwatch.StartNew()
    { new ITimeTracker with        
        member x.Elapsed = sw.Elapsed
        member x.ElapsedString = x.Elapsed.ToString("h':'mm':'ss'.'FFF")
        member x.Dispose() =
            printfn $"Completed in +{x.ElapsedString}"
            printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "

    }

