open System
open AoC.Common

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 10 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    let inputText = System.IO.File.ReadLines(filename)
    use diag = Utility.GetTracker ()

    let dist = -1

    printfn ""
    printfn $"Final dist = {dist:``#,#``} ({dist})"
    printfn ""
    0
