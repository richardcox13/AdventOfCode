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





let resSeq = findPossiblePartNumbers inputArray
            |> Seq.take 10 (**** DON'T FORGET THIS ****)
            |> Seq.toList
//resSeq |> Seq.iter (fun (row, col, chr) -> printf $"({row},{col}) '{chr}' ")
resSeq |> Seq.iter (fun x -> printf $"%A{x}")
printfn ""
//printfn $"Result = {resSeq |> Seq.sum}"
