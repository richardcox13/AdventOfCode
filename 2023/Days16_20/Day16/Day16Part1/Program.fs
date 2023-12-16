open System
open System.Collections.Generic
open System.IO
open AoC.Common

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 16 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    use diag = Utility.GetTracker ()

    let input = System.IO.File.ReadLines(filename)


    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
