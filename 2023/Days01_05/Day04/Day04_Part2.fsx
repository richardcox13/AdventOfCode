//
// Day 4 Part 2
//
// Usage
//  dotnet fsi .\Day04_Part1.fsx <input-file>

open System
open System.IO
open System.Text.RegularExpressions

type CardStare = {
    CardNumber: int
    Score: int
    mutable Count: int
}

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
    { CardNumber = cardNo; Score = matchCount; Count = 1 }

let filename = fsi.CommandLineArgs[1]
let textReader = File.ReadLines(filename)

//printfn ""
//printfn "Initial scores:"
let cards = textReader |> Seq.map (fun l -> processCard l)
                        |> Seq.toArray
//cards |> Seq.iter (fun c -> printfn $"  #{c.CardNumber}: score {c.Score}")

//printfn ""
//printfn "Updating score counts"
for cn in 1..(cards.Length) do
    let c = cards[cn-1]
    assert (c.CardNumber = cn)

    if c.Score > 0 then
        for x in 1..(c.Score) do
            let other = cards[cn-1+x]
            other.Count <- other.Count + c.Count

//cards |> Seq.iter (fun c -> printfn $"  #{c.CardNumber}: score {c.Score}, count: {c.Count}")

printfn ""
printfn $"Result = {cards |> Seq.sumBy _.Count}"
