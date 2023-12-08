open System

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    let filename = args[0]
    printfn $"Input file {filename}"
    let sw = System.Diagnostics.Stopwatch.StartNew ()



    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    9
