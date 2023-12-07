open System

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    let filename = args[0]
    printfn $"Reading from {filename}"

    let sw = System.Diagnostics.Stopwatch.StartNew ()


    let result = -1

    sw.Stop()
    printfn ""
    printfn $"Result = {result}"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    0
