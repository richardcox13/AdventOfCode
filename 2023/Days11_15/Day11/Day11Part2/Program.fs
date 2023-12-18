open System
open AoC.Common
open AoC.Common.Core

let galaxy = '#'
let expansionFactor = 1_000_000

let printMap (map: string[]) (title : string)=
    printfn ""
    printfn $"{title}:"
    for r = 0 to (map.Length-1) do
        printfn $"\t{map[r]}"
    ()

let findEmptyRows (map: string[]) =
    seq {
        for r = 0 to (map.Length-1) do
            let row = map[r]
            if row.IndexOf(galaxy) = -1 then
                yield r
    } |> Seq.toArray

let findEmptyColumns (map: string[]) =
    seq {
        for col = 0 to (map[0].Length-1) do
            let colHasGalaxy
                = seq { 0 .. (map.Length-1) }
                  |> Seq.map (fun row -> map[row][col])
                  |> Seq.exists (fun c -> c = galaxy)
            if not colHasGalaxy then
                yield col
    } |> Seq.toArray

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

let applyExpansion expansionRows expansionColumns (galaxy: Position)=
    let rowsToExpand = expansionRows |> Seq.where (fun r -> r < galaxy.Row) |> Seq.length
    let colsToExpand = expansionColumns |> Seq.where (fun c -> c < galaxy.Col) |> Seq.length
    let ef = expansionFactor - 1
    let res = {
        Row = galaxy.Row + (rowsToExpand * ef)
        Col = galaxy.Col + (colsToExpand * ef)
    }
    printfn $"\texpanded {galaxy} to {res} with {rowsToExpand} rows and {colsToExpand} cols expanded"
    res

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
    printfn ""

    let galaxies
        = map
          |> extractPositions galaxy
          |> Seq.toArray
    let gg = galaxies |> Seq.map (fun g -> $"{g}") |> String.concat ", "
    printfn $"Found {galaxies.Length} galaxies: {gg}"

    let emptyRows = findEmptyRows map
    let emptyCols = findEmptyColumns map
    let sep = ", "
    printfn $"There are {emptyRows.Length} empty rows: {emptyRows |> Array.map (fun x -> x.ToString()) |> String.concat sep}"
    printfn $"There are {emptyCols.Length} empty columns: {emptyCols |> Array.map (fun x -> x.ToString()) |> String.concat sep}"

    let expandedGalaxies
        = galaxies
          |> Seq.map (fun g -> applyExpansion emptyRows emptyCols g)
          |> Seq.toArray
    let gg = expandedGalaxies |> Seq.map (fun g -> $"{g}") |> String.concat ", "
    printfn $"Expanded galaxies: {gg}"
    let galaxyPairs = expandedGalaxies |> getAllDistinctPairs |> Seq.toArray

    let distanaces
        = galaxyPairs
          |> Seq.map (fun (left, right) ->
                let dist = int64 (abs (left.Row - right.Row) + abs (left.Col - right.Col))
                //printfn $"\tFrom {left} to {right} dist = {dist}"
                dist
          )
    
    let result = distanaces |> Seq.sum
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
