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
    // Keep track of (position * remaining-steps) already visited.
    // No mutable Set<T>, so stop bools...
    let visitedCache = new Dictionary<Position * int, bool>()

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
                    let (cached, _) = visitedCache.TryGetValue((pos, remaining))

                    if cached then
                        // Been here before with more steps left... so drop back
                        yield! Seq.empty<Position>
                    else
                        visitedCache[(pos, remaining)] <- true
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
    let stepCount = Int32.Parse(args[1])
    printfn $"Input file {filename}; steps = {stepCount}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let start = findChar 'S' input
    printfn $"Starting at {start}"

    let steps = takeSteps input start stepCount

    //for (idx,s) in (steps |> Seq.mapi (fun idx p -> idx,p)) do
    //    printfn $"#{idx+1} Reached {s}"

    let result = steps |> Seq.length
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
