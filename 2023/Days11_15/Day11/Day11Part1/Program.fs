open System
open AoC.Common

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 11 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    let input= System.IO.File.ReadAllLines(filename)
    use diag = Utility.GetTracker ()


    let result = -1
    printfn ""
    printfn $"Internal tile count = {result:``#,0``} ({result})"
    printfn ""
    0
