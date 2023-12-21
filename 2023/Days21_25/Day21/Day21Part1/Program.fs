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


    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
