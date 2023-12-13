open System
open AoC.Common

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 13 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    let input = System.IO.File.ReadLines(filename)
    use diag = Utility.GetTracker ()

    //showSomeInputStats input

    let result = -1

    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
