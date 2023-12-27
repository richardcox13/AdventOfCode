open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open AoC.Common
open AoC.Common.Core

type Direction =
    | Up = 1
    | Down = 2
    | Left = 3
    | Right = 4

let showPath (title: string) (input: string[]) (path: Position list) =
    let grid = Array2D.init (input.Length) (input[0].Length) (fun r c -> input[r][c])
    path
        |> List.iter (fun p ->
                let c = grid[p.Row, p.Col]
                if c = '#' then
                    printfn $"Overwriting forest at {p}"
                else if c = 'o' then
                    printfn $"Duplicate path step at {p}"
                grid[p.Row, p.Col] <- 'o'
            )
    Console.WriteLine(title)
    for row in 0 .. ((Array2D.length1 grid) - 1) do
        Console.Write("  ")
        for col in 0 .. ((Array2D.length2 grid) - 1) do
            Console.Write(grid[row, col])
        Console.WriteLine ()
    Console.WriteLine ()


let findPaths (input: string[]) =
    let maxRow = input.Length - 1
    let maxCol = input[0].Length - 1

    let startPos= { Row = 0; Col = 1 }
    let endPos = { Row = maxRow; Col = maxCol - 1 }

    let getTile row col = input[row][col]

    seq {
        // (history, row, col, direction, count)
        let stack = new Stack<(int * int) list * int * int * Direction * int>()
        stack.Push([], startPos.Row, startPos.Col, Direction.Down, 0)

        while stack.Count > 0 do
            let (history, newRow, newCol, inboundDirection, count) = stack.Pop()
            if newRow = endPos.Row && newCol = endPos.Col then
                yield (count, history)
            else if not (List.contains (newRow,newCol) history) then
                let c = getTile newRow newCol
                let doIterate = c <> '#'
                if doIterate then
                    let h = (newRow,newCol) :: history
                    let cc = count+1
                    //showPath $"Depth {count}" input h
                    if inboundDirection <> Direction.Down && newRow > 0 then
                        stack.Push((h, (newRow-1), newCol, Direction.Up, cc))
                    if inboundDirection <> Direction.Up && newRow < maxRow then
                        stack.Push((h, (newRow+1), newCol, Direction.Down, cc))
                    if inboundDirection <> Direction.Right && newCol < maxCol then
                        stack.Push((h,  newRow, (newCol-1), Direction.Left, cc))
                    if inboundDirection <> Direction.Left && newCol > 0 then
                        stack.Push((h, newRow, (newCol+1), Direction.Right, cc))
    }


[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 23 Part 2"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let histToStr (hist: (int*int) list) =
        hist |> List.map (fun (r,c)-> $"({r},{c})") |> String.concat ", "

    let results
        = findPaths input
          //|> Seq.take 1
          |> Seq.sortByDescending (fun (c, _) -> c)
          |> Seq.toArray

    let res = results[0]
    printfn $"Longest path is {fst res} strps"
    printfn $"Steps: {snd res |> List.rev |> histToStr}"
    printfn ""
    printfn "Other solution lengths: %s"
        (results |> Seq.skip 1 |> Seq.map (fun (c,_) -> c.ToString()) |> String.concat ", ")

    //for (count, hist) in results do
    //    let h = hist |> List.rev
    //    printfn $"{count} steps found: {histToStr h}"
    //    printfn ""
    //    //showPath "***** At end" input h

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
