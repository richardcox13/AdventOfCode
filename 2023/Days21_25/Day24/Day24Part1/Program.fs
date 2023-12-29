open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open AoC.Common
open AoC.Common.Core

type Line2D =
    {
        Index: int
        StartX: double
        StartY: double
        VelX: double
        VelY: double
    }

let parseInput (input: string seq) =
    let matchNumber = new Regex(@"-?\d+")
    input
    |> Seq.mapi (fun idx line ->
        let ms = matchNumber.Matches(line)
        assert (ms.Count = 6)

        // For part 1, only considering X & Y so 3rd & 6th matches ignored
        { Index = idx;
          StartX = Double.Parse(ms[0].Value);
          StartY = Double.Parse(ms[1].Value);
          VelX = Double.Parse(ms[3].Value);
          VelY = Double.Parse(ms[4].Value); }
    )

let allPairs<'T> (collection: 'T array) =
    let max = collection.Length-1
    seq {
        for first in 0 .. (max-1) do
            for second in (first+1) .. max do
                yield (collection[first], collection[second])
    }

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 24 Part 1"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let lines = input |> parseInput |> Seq.toArray

    for (f, s) in (lines |> allPairs) do
        printfn $"({f.Index}, {s.Index})"

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
