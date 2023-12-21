open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open AoC.Common
open AoC.Common.Core

let findChar (target: char) (grid: string[]) =
    let (row, col)
        = seq { 0 .. (grid.Length-1) }
          |> Seq.map (fun row -> row, (grid[row].IndexOf(target)))
          |> Seq.where (fun (_,col) -> col <> -1)
          |> Seq.exactlyOne
    { Row = row; Col = col }

let takeSteps (grid: string[]) start stepCount =
    let rec inner remaining pos =
        seq {
            if pos.Row >= 0
                    && pos.Row < grid.Length
                    && pos.Col >= 0
                    && pos.Col < grid[0].Length
                    && grid[pos.Row][pos.Col] <> '#' then
                if remaining = 0 then
                    yield pos
                else
                    yield! (inner (remaining-1) (Pos.moveUp pos))
                    yield! (inner (remaining-1) (Pos.moveDown pos))
                    yield! (inner (remaining-1) (Pos.moveLeft pos))
                    yield! (inner (remaining-1) (Pos.moveRight pos))
        }
    inner stepCount start |> Seq.distinct

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 21 Part 1"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let start = findChar 'S' input
    printfn $"Starting at {start}"

    let steps = takeSteps input start 6
    for (idx,s) in (steps |> Seq.mapi (fun idx p -> idx,p)) do
        printfn $"#{idx+1} Reached {s}"

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
