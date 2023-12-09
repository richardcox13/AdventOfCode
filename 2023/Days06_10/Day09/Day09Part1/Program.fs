open System

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn ""
    printfn $"Day 9 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    let inputText = System.IO.File.ReadLines(filename)

    let sw = System.Diagnostics.Stopwatch.StartNew ()


    let sum = -1

    printfn $"Final count = {sum:``#,#``} ({sum})"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    9
