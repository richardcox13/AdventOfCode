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
                             | a -> failwith $"Findiung bounadaries: unecpected input '{a} {dist}' at input index {inputIdx}"
            let newLimits = { MinRow = min newPos.Row limits.MinRow;
                                        MaxRow = max newPos.Row limits.MaxRow;
                                        MinCol = min newPos.Col limits.MinCol;
                                        MaxCol = max newPos.Col limits.MaxCol }
            inner (inputIdx+1) newPos newLimits

    let maxLimits = inner 0 { Row=0; Col=0 } { MinRow=0; MaxRow=0; MinCol=0; MaxCol=0 }
    // as we started at (0,0) cnnot be +ve
    assert (maxLimits.MinRow <= 0)
    assert (maxLimits.MinCol <= 0)
    let startPos = { Row = -1 * maxLimits.MinRow; Col = -1 * maxLimits.MinCol }
    let rows = maxLimits.MaxRow - maxLimits.MinRow + 1
    let cols = maxLimits.MaxCol - maxLimits.MinCol + 1
    (rows, cols, startPos)


let getOutline (inp: string seq) gridRows gridCols startPos =
    let input = inp |> Seq.toArray

    let grid = Grid.create gridRows gridCols false

    let fillHoizontal rowIdx cols = 
        for c in cols do
            grid[rowIdx,c] <- true

    let fillVertical rows colIdx =
        for r in rows do
            grid[r,colIdx] <- true

    let rec inner inputIdx curPos =
        if inputIdx >= input.Length then
            ()
        else
            let ss = input[inputIdx].Split(' ')
            assert (ss.Length >= 2)
            let dir = ss[0]
            let dist = Int32.Parse(ss[1])
            let nextPos = match dir with
                             | "D" ->
                                let (s, e) = curPos.Row, curPos.Row + dist
                                fillVertical (seq {s .. e}) curPos.Col
                                { curPos with Row = e}
                             | "U" ->
                                let (s, e) = curPos.Row - dist, curPos.Row
                                fillVertical (seq {s .. e}) curPos.Col
                                { curPos with Row = s}
                             | "R" ->
                                let (s, e) = curPos.Col, curPos.Col + dist
                                fillHoizontal curPos.Row (seq {s .. e})
                                { curPos with Col = e }
                             | "L" ->
                                let (s, e) = curPos.Col - dist, curPos.Col
                                fillHoizontal curPos.Row (seq {s .. e})
                                { curPos with Col = s }
                             | a -> failwith $"Filling bounadaries: unecpected input '{a} {dist}' at input index {inputIdx}"
            inner (inputIdx+1) nextPos
    inner 0 startPos
    grid


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
    let (gridRows, gridCols, startPos) = findGridSize input
    printfn $"The grid needs to be {gridRows} x {gridCols}, with a starting position {startPos}"

    let grid = getOutline input gridRows gridCols startPos
    Grid.printf "With bnoundaries" "    " (fun b -> if b then "#" else ".") grid

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
