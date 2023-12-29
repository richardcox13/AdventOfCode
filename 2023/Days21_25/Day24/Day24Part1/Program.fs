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

// Returns double option: if None no crossing otherwise the time.
let getTimeOfCrossing left right =
    let xDenom = right.VelX - left.VelX
    let xNum = left.StartX - right.StartY
    if xDenom <> 0 && xNum <> 0 then
        Some (xNum / xDenom)
    else
        let yDenom = right.VelY - left.VelY
        let yNum = left.StartY - right.StartX
        if yDenom <> 0 && yNum <> 0 then
            Some (yNum / yDenom)
        else
            None

let getCrossing left right =
    let tt = getTimeOfCrossing left right
    if tt.IsNone then
        None
    else
        let t = tt.Value
        if t > 0 then
            let x = left.StartX + t * left.VelX
            let y = left.StartY + t * left.VelY

            let altX =  right.StartX + t * right.VelX
            assert ((x - altX) < (Double.Epsilon * x))
            let altY = right.StartY + t * right.VelY
            assert (((y - altY)) < (Double.Epsilon * y))

            Some (x, y)
        else
            None

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 24 Part 1"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    let lowCoord = Double.Parse(args[1])
    let highCoord = Double.Parse(args[2])
    printfn $"Looking for crossings in [{lowCoord}, {highCoord}]"
    printfn ""

    let lines = input |> parseInput |> Seq.toArray


    let count
        = lines
          |> allPairs
          |> Seq.map (fun (left, right) -> left, right, (getCrossing left right))
          |> Seq.iter (fun (left, right, crossing) ->
                if crossing.IsNone then
                    printfn $"Lines {left.Index} and {right.Index}: do not cross"
                else
                    let c = crossing.Value
                    printfn $"Lines {left.Index} and {right.Index}: cross at ({fst c}, {snd c})"
            )

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
