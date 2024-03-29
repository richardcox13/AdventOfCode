﻿open System

type Hand =
    {
        Cards: string
        CardValues: sbyte[]
        // Could use an enum, but once classified this is only compared
        Type: int
        Bid: int
    }


let readFile filename =
    let reader = System.IO.File.ReadLines(filename)
    reader |> Seq.map (fun l ->
                  let x = l.Split(' ') |> Array.map _.Trim()
                  x[0], Int32.Parse(x[1])
              )

let makeCard (hand: string, bid) =
    let cardScore (card: char) =
        match card with
        | 'A' -> 14y
        | 'K' -> 13y
        | 'Q' -> 12y
        | 'J' -> 11y
        | 'T' -> 10y
        | c when c >= '2' && c <= '9'
            -> (sbyte c) - (sbyte '0')
        | _ as c -> failwith $"Unknown card '{c}'"

    let handType (h:string) =
        let grps = h |> Seq.groupBy (fun c -> c)
                            |> Seq.map (fun (_, cs) -> cs |> Seq.length)
                            |> Seq.sortDescending
                            |> Seq.toList
        match grps with
        | [ 5 ] -> 7          // 5 of a kind
        | [ 4; 1 ] -> 6       // 4 of a kind
        | [ 3; 2 ] -> 5       // full house
        | [ 3; 1; 1 ] -> 4    // 3 of a kind
        | [ 2; 2; _] -> 3     // Two pairs
        | [ 2; 1; 1; 1 ] -> 2 // A pair
        | _ -> 1              // High card

    let cardVals = hand |> Seq.map (fun c -> cardScore c) |> Seq.toArray
    { Cards = hand; CardValues = cardVals; Type = handType hand; Bid = bid }

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    let filename = args[0]
    printfn $"Input file {filename}"
    let sw = System.Diagnostics.Stopwatch.StartNew ()

    let cards =
        readFile filename
        |> Seq.map makeCard
        |> Seq.sortWith (fun a b ->
                            let c = compare a.Type b.Type
                            if c <> 0 then
                                c
                            else
                                compare a.CardValues b.CardValues
                        )
        |> Seq.mapi (fun idx card -> card, idx, (idx+1) * card.Bid)
        |> Seq.toArray
    for (c, idx, value) in cards do
        printfn $"#{idx} {{ Cards=\"{c.Cards}\"; Type={c.Type}; CVs=%A{c.CardValues} Bid={c.Bid}  ({value:``#,#``})}}"

    let score =
        cards
        |> Seq.sumBy (fun (_, _, v) -> v)

    sw.Stop()
    printfn ""
    printfn $"Result = {score:``#,#``}"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    0
