//
// Day 3 Part 1
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

let isSymbolInRun (input: string[]) row col length =
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
                not (Char.IsAsciiDigit(c) || c = '.' )
            )

let isSymbolFound input row col length =
    (isSymbolInRun input (row-1) col length)
        || (isSymbolInRun input row col length)
        || (isSymbolInRun input (row+1) col length)

let resSeq = findPossiblePartNumbers inputArray
            //|> Seq.take 10 (**** DON'T FORGET THIS ****)
            |> Seq.where (fun (row, col, value) -> isSymbolFound inputArray row col (value.Length))
            |> Seq.toList
//resSeq |> Seq.iter (fun (row, col, chr) -> printf $"({row},{col}) '{chr}' ")
resSeq |> Seq.iter (fun x -> printf $"%A{x}")
printfn ""

let res = resSeq |> Seq.map (fun (_, _, value) -> Int32.Parse(value))
                 |> Seq.sum
printfn $"Result = {res}"
