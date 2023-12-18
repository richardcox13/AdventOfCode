open System
open System.IO
open AoC.Common
open AoC.Common.Core

type GridLimits = {
    MinRow: int
    MaxRow: int
    MinCol: int
    MaxCol: int
}

// Return (rows, cols)
let findGridSize (inp: string seq) =
    let input = inp |> Seq.toArray
    let rec inner inputIdx (curPos: Position) (limits: GridLimits) =
        if inputIdx >= input.Length then
            limits
        else
            let ss = input[inputIdx].Split(' ')
            assert (ss.Length >= 2)
            let dir = ss[0]
            let dist = Int32.Parse(ss[1])
            let newPos = match dir with
                             | "D" -> { curPos with Row = curPos.Row + dist }
                             | "U" -> { curPos with Row = curPos.Row - dist }
                             | "R" -> { curPos with Col = curPos.Col + dist }
                             | "L" -> { curPos with Col = curPos.Col - dist }
                             | a -> failwith $"Unecpected input '{a} {dist}' at input index {inputIdx}"
            let newLimits = { MinRow = min newPos.Row limits.MinRow;
                                        MaxRow = max newPos.Row limits.MaxRow;
                                        MinCol = min newPos.Col limits.MinCol;
                                        MaxCol = max newPos.Col limits.MaxCol }
            inner (inputIdx+1) newPos newLimits

    let maxLimits = inner 0 { Row=0; Col=0 } { MinRow=0; MaxRow=0; MinCol=0; MaxCol=0 }
    let rows = maxLimits.MaxRow - maxLimits.MinRow + 1
    let cols = maxLimits.MaxCol - maxLimits.MinCol + 1
    (rows, cols)

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 18 Part 1"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadLines(filename)
    printfn ""
    let (gridRows, gridCols) = findGridSize input
    printfn $"The grid needs to be {gridRows} x {gridCols}"


    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
