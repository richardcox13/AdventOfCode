//
// Day 4 Part 1
//
// Usage
//  dotnet fsi .\Day04_Part1.fsx <input-file>

open System
open System.IO
open System.Text.RegularExpressions

// Return (cardNo: int, winingNos: int list[], cardsNos: int[])
let breakInputLine (inputLine: string) =
    let extractIntSet (input:string) =
        input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map(fun x -> Int32.Parse(x.Trim()))
            |> Set.ofSeq

    // All lines start "Card\s+\d+:"
    let input = inputLine.Substring(5)
    let cardSplit = input.Split(':')
    assert (cardSplit.Length = 2)
    let cardNo = Int32.Parse(cardSplit[0].Trim())

    let nosSplit = cardSplit[1].Split('|')

    let winningNos = extractIntSet nosSplit[0]
    let cardsNos = extractIntSet nosSplit[1]

    cardNo, winningNos, cardsNos


// Return the score of this care
let processCard inputLine =
    let (cardNo, winningNos, cardsNos) = breakInputLine inputLine
    //printfn $"Card {cardNo}, winners={winningNos}, cards numbers={cardsNos}"
    let intersection = Set.intersect winningNos cardsNos
    let matchCount = intersection |> Seq.length
    //printfn $"\tIntersection={intersection} (count: {matchCount})"
    if matchCount > 0 then
        int (2.0 ** (double matchCount - 1.0))
    else
        0

let filename = fsi.CommandLineArgs[1]
let textReader = File.ReadLines(filename)

printfn ""
let resSeq = textReader |> Seq.map (fun l -> processCard l)
                        |> Seq.toList
//resSeq |> Seq.iter (fun res -> printf $"{res} ")
printfn ""
printfn $"Result = {resSeq |> Seq.sum}"
