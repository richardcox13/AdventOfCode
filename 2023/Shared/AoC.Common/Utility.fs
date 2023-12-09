module AoC.Common.Utility

open System
open System.Diagnostics

type public TimeTracker =
    { sw: Stopwatch }
    member x.Elapsed = x.sw.Elapsed
    member x.ElapsedString = x.Elapsed.ToString("h':'mm':'ss'.'FFF")
    interface IDisposable with
        member x.Dispose() =
            printfn $"Completed in +{x.ElapsedString}"
            printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "

let public GetTracker () =
    { sw = Stopwatch.StartNew() }
