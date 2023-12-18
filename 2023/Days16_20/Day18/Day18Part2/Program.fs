open System
open System.Collections.Generic
open System.IO
open AoC.Common
open AoC.Common.Core

// Part 1 pasring for easier debugging...
// Return (direction, distance)
let parseLine (line : string)=
    let ss = line.Split(' ')
    assert (ss.Length >= 2)
    let dir = ss[0]
    let dist = Int32.Parse(ss[1])
    dir, dist

let parseInputIntoPoints (input: string[]) =
    let rec inner inpIdx curPos =
        seq {
            if inpIdx < input.Length then
                let line = input[inpIdx]
                let (dir, dist) = parseLine line
                let newPos = match dir with
                                | "D" -> { curPos with Row = curPos.Row + dist }
                                | "U" -> { curPos with Row = curPos.Row - dist }
                                | "R" -> { curPos with Col = curPos.Col + dist }
                                | "L" -> { curPos with Col = curPos.Col - dist }
                                | a -> failwith $"Unecpected input '{a} {dist}' at input index {inpIdx}"
                yield newPos
                yield! (inner (inpIdx+1) newPos)
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

    (abs (part1 + crossTerms)) / 2

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 18 Part 2"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let points
        = parseInputIntoPoints input
          |> Seq.toArray

    for (idx, p) in (points |> Seq.mapi (fun idx p -> (idx, p))) do
        printfn $"#{idx}: {p}"

    let result = shoelace points
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
