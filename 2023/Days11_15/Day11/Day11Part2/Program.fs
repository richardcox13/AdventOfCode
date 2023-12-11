open System
open AoC.Common
open AoC.Common.Core

let galaxy = '#'

let printMap (map: string[]) (title : string)=
    printfn ""
    printfn $"{title}:"
    for r = 0 to (map.Length-1) do
        printfn $"\t{map[r]}"
    ()

let expandRows (map: string[]) =
    seq {
        for r = 0 to (map.Length-1) do
            let row = map[r]
            yield row
            if row.IndexOf(galaxy) = -1 then
                yield row
    } |> Seq.toArray

let expandColumns (map: string[]) =
    let invertMap (map: string[]) =
        Array.init (map[0].Length)
                (fun col ->
                                seq {
                                    for row in 0 .. (map.Length-1) -> map[row][col]
                                } |> String.Concat
                           )

    map |> invertMap |> expandRows |> invertMap

let extractPositions targetChar (map: string[]) =
    seq {
        for r in 0 .. (map.Length-1) do
            for c in 0 .. (map[0].Length-1) do
                if map[r][c] = targetChar then
                    yield { Row = r; Col = c }
    }

let getAllDistinctPairs input =
    let len = input |> Array.length
    seq {
        for outer in 0 .. (len-1) do
            for inner in (outer+1) .. (len-1) do
                yield input[outer], input[inner]
    }

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 11 Part 2"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    let map= System.IO.File.ReadAllLines(filename)
    use diag = Utility.GetTracker ()

    printMap map "Start"

    let mapExpanded = map |> expandRows |> expandColumns
    printMap mapExpanded "Fully expanded"
    printfn ""
    printfn $"Originally {map.Length} rows x {map[0].Length} columns; now {mapExpanded.Length} rows x {mapExpanded[0].Length} cols"

    printfn ""
    let galaxies
        = mapExpanded
          |> extractPositions galaxy
          |> Seq.toArray
    let gg = galaxies |> Seq.map (fun g -> $"{g}") |> String.concat ", "
    printfn $"Found {galaxies.Length} galaxies: {gg}"

    let galaxyPairs = galaxies |> getAllDistinctPairs |> Seq.toArray
    printfn $"Found {galaxyPairs  |> Array.length} pairs"

    let distanaces
        = galaxyPairs
          |> Seq.map (fun (left, right) ->
                let dist = abs (left.Row - right.Row) + abs (left.Col - right.Col)
                printfn $"\tFrom {left} to {right} dist = {dist}"
                dist
          )
    
    let result = distanaces |> Seq.sum
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
