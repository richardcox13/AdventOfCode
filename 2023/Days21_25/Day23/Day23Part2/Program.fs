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

    let getTile pos = input[pos.Row][pos.Col]

    let rec iterate history newPos inboundDirection count =
        seq {
            if newPos.Row < 0
                    || newPos.Row > maxRow
                    || newPos.Col < 0
                    || newPos.Col > maxCol then
                None
            else if newPos = endPos then
                Some (count, history)
            else if List.contains newPos history then
                None
            else
                let c = getTile newPos
                let doIterate = match c, inboundDirection with
                                     | ('#', _) -> false
                                     | ('>', d) when d <> Direction.Right -> false
                                     | ('<', d) when d <> Direction.Left -> false
                                     | ('^', d) when d <> Direction.Up -> false
                                     | ('v', d) when d <> Direction.Down -> false
                                     | _ -> true
                if doIterate then
                    let h = newPos :: history
                    //showPath $"Depth {count}" input h
                    yield! iterate h (Pos.moveUp newPos) Direction.Up (count+1)
                    yield! iterate h (Pos.moveDown newPos) Direction.Down (count+1)
                    yield! iterate h (Pos.moveLeft newPos) Direction.Left (count+1)
                    yield! iterate h (Pos.moveRight newPos) Direction.Right (count+1)
                else
                    None
        }

    iterate [] startPos Direction.Down 0
        |> Seq.where (fun res -> res.IsSome)
        |> Seq.map (fun res -> res.Value)


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

    let histToStr hist =
        hist |> List.map (fun x -> x.ToString()) |> String.concat ", "

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
