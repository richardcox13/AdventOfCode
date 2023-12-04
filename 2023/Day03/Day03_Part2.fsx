//
// Day01 Part2
//
// Usage
//  dotnet fsi .\Day02_Part2.fsx <input-file>

open System
open System.IO
open System.Text.RegularExpressions


let filename = fsi.CommandLineArgs[1]
// Get a string[]
let inputArray = File.ReadAllLines(filename)

type  ScannerState = {
    Col: int
    Accumulator: string
    NewValue: string
}

let findPossiblePartNumbers (input: string[]) =
    let rowCount = inputArray.Length
    assert (rowCount > 3)
    let rowLength = inputArray[0].Length
    seq {
        for row in 0..(rowCount-1) do
            yield! seq { 0..rowLength }
                    |> Seq.scan (fun state col ->
                            if col = rowLength then
                                //printfn $"({row},{col}): EoL"
                                // If number ends at the end of the row...
                                { state with NewValue = state.Accumulator; Accumulator = ""}
                            else
                                let thisChar = input[row][col]
                                //printf $"({row},{col}): '{thisChar}'"
                                if Char.IsAsciiDigit(thisChar) then
                                    let s = $"%c{thisChar}"
                                    let c = if state.Accumulator = "" then col else state.Col
                                    { Col = c; Accumulator = state.Accumulator + s; NewValue = "" }
                                else if state.Accumulator.Length > 0 then
                                    // Yield a complete number
                                    { state with NewValue = state.Accumulator; Accumulator = ""}
                                else
                                    // Nothing to yield. nothing to accumulate
                                    { Col = col; Accumulator = ""; NewValue = "" }
                        ) { Col = -1; Accumulator = ""; NewValue = "" }
                        // |> Seq.map (fun s ->
                        //                  printfn $"%A{s}"
                        //                  s
                        // )
                        |> Seq.where (fun s -> s.NewValue <> "")
                        |> Seq.map (fun s -> row, s.Col, s.NewValue)
    }

let isAsteriskInRun (input: string[]) row col length =
    if row < 0 || row >= input.Length then
        // Nothing to find on a row that does not exist!
        false
    else
        let inp = input[row]
        // We check one char before the beginning of the number
        let first = Math.Max(0, col-1)
        // This is inclusive, but looking at char after number
        let last = Math.Min(col+length, inp.Length-1)

        seq { first..last }
            |> Seq.exists (fun col ->
                let c = inp[col]
                c = '*' 
            )

let isAsteriskAdjacent input row col length =
    (isAsteriskInRun input (row-1) col length)
        || (isAsteriskInRun input row col length)
        || (isAsteriskInRun input (row+1) col length)

let findAllAsterisks (input: string[]) =
    let rows = input.Length
    let cols = input[0].Length
    seq {
        for r in 0..(rows-1) do
            for c in 0..(cols-1) do
                if input[r][c] = '*' then
                    yield (r, c)
    }

let numbers = findPossiblePartNumbers inputArray
            //|> Seq.take 10 (**** DON'T FORGET THIS ****)
            |> Seq.where (fun (row, col, value) -> isAsteriskAdjacent inputArray row col (value.Length))
            |> Seq.toList
//resSeq |> Seq.iter (fun x -> printf $"%A{x}")
printfn ""
printfn $"There are {numbers |> List.length} numbers next to a '*'"
printfn ""
let asterisks = findAllAsterisks inputArray |> Seq.toList
printfn $"There are {asterisks |> List.length} asterisks"
printfn ""

let gearPairs = asterisks
                |> Seq.map (fun (aRow, aCol) ->
                    let foundInRow r = numbers |> Seq.exists (fun (nRow, _, _)  -> nRow = r)
                    // Find all the numbers adjacent to this asterisk
                    let verticallyAdjacent =
                        numbers |> Seq.where (fun (nRow, _, _) ->
                                                nRow >= aRow-1 && nRow <= aRow+1
                                             )
                    let adjacent =
                        verticallyAdjacent
                            |> Seq.where (fun (_, nCol, value) ->
                                            let first = nCol
                                            let last = nCol + value.Length - 1

                                            first <= (aCol+1) && last >= (aCol-1)
                                         )
                            |> Seq.toArray
                    let l = adjacent |> Array.length
                    if l > 2 then
                        printfn $"Have %d{l} numbers adjacent to the same asterisk (%d{aRow},%d{aCol}): %A{adjacent}"
                    adjacent
                )
                |> Seq.where (fun adj -> (adj |> Array.length) = 2)
                |> Seq.toList

printfn $"There are {gearPairs |> Seq.length} gear pairs"
// printfn ""
// for pair in gearPairs do
//     printfn "[%s]" (pair |> Array.map (fun (_, _, value) -> value) |> String.concat "-")

printfn ""

let res = gearPairs
                |> Seq.map (fun pair -> pair |> Array.map (fun (_, _, value) -> value))
                |> Seq.map (fun pair -> Int32.Parse(pair[0]) * Int32.Parse(pair[1]))
                |> Seq.sum
printfn $"Result = {res}"
