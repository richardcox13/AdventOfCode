﻿open System
open System.Collections.Generic
open System.IO
open AoC.Common
//open AoC.Common.Core

[<CustomEquality>][<NoComparison>]
type Position =
    { Row: int64; Col: int64 }
    override x.ToString() = $"({x.Row}, {x.Col})"
    override x.GetHashCode() = HashCode.Combine(x.Row, x.Col)
    override left.Equals right =
        match right with
        | :? Position as r when left.Row = r.Row && left.Col = r.Col
            -> true
        | _ -> false

// Part 1 pasring for easier debugging...
// Return (direction, distance)
let parseLine (line : string)=
    let ss = line.Split(' ')
    assert (ss.Length >= 3)
    let x = ss[2].TrimStart('(').TrimEnd(')')
    assert (x[0] = '#')
    let dir = match x[6] with
                 | '0' -> "R"
                 | '1' -> "D"
                 | '2' -> "L"
                 | '3' -> "U"
                 | x -> failwith $"Unecpected direction '{x}' in input \"{line}\""
    let dist = Int64.Parse(x[1..5], System.Globalization.NumberStyles.HexNumber)
    dir, dist

// Returns (dir * dist) seq
let getInstructions (input: string seq) =
    input
      |> Seq.map (fun line-> parseLine line)

let parseInputIntoPoints (instrctions: (string * int64)[]) =
    let rec inner instrIdx curPos =
        seq {
            if instrIdx < instrctions.Length then
                let (dir, dist) = instrctions[instrIdx]
                let newPos = match dir with
                                // Lines are one thick, so make adjustment to enclose on
                                // right and below
                                | "D" -> { curPos with Row = curPos.Row + dist }
                                | "U" -> { curPos with Row = curPos.Row - dist }
                                | "R" -> { curPos with Col = curPos.Col + dist }
                                | "L" -> { curPos with Col = curPos.Col - dist }
                                | a -> failwith $"Unecpected input '{a} {dist}' at input index {instrIdx}"
                yield newPos
                yield! (inner (instrIdx+1) newPos)
        }
    
    // Input gives a closed loop, no need to yield the start
    // as it will appear at the end
    inner 0 { Row = 0; Col = 0 }

let shoelace (points: Position array) =
    // Summations normally shown separately, but can avoid multiple loops
    let n = points.Length // formulae use n...

    let part1
        =  seq { 0 .. (n-2) }
           |> Seq.map (fun i -> points[i].Col * points[i+1].Row
                                            - points[i+1].Col * points[i].Row)
           |> Seq.sum

    let crossTerms = points[n-1].Col * points[0].Row
                            - points[0].Col * points[n-1].Row

    (abs (part1 + crossTerms)) / 2L

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 18 Part 2"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    //let input = [| "R 3"; "D 3"; "L 3"; "U 3" |]
    //let input = [| "R 5"; "D 3"; "L 2"; "D 2"; "L 3"; "U 5" |]
    printfn ""

    let instructions = getInstructions input |> Seq.toArray

    let points
        = parseInputIntoPoints instructions
          |> Seq.toArray

    //for (idx, p) in (points |> Seq.mapi (fun idx p -> (idx, p))) do
    //    printfn $"#{idx}: {p}"

    let shoelaceArea = shoelace points
    let boundaryArea
        = instructions
          |> Seq.map (fun (_, dist) -> dist)
          |> Seq.sum

    let result = int64 ((float shoelaceArea) + (float boundaryArea)/2.0 + 1.0)
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
