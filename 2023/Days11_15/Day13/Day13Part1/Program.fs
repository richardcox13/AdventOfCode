open System
open System.Collections.Generic
open AoC.Common

let parseInput filename =
    let input = System.IO.File.ReadLines(filename)
    seq {
        let mutable acc = new List<string>()
        for line in input do
            if String.IsNullOrWhiteSpace(line) then
                if acc.Count > 0 then
                    yield (acc |> Seq.toArray)
                    acc <- new List<string>()
            else
                acc.Add(line)
        if acc.Count > 0 then
            yield (acc |> Seq.toArray)
    }


[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 13 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    use diag = Utility.GetTracker ()

    let patterns = parseInput filename |> Seq.toArray
    printfn $"There are {patterns.Length} patterns"

    let result = -1

    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
