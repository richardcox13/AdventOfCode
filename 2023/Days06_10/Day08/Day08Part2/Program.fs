open System
open System.Text.RegularExpressions

let makeMap (input: string seq) =
    let data
        = input
          |> Seq.map (fun l ->
                                let ms = Regex.Match(l, @"(?<loc>[A-Z0-9]{3}) = \((?<left>[A-Z0-9]{3}), (?<right>[A-Z0-9]{3})\)")
                                assert ms.Success
                                assert (ms.Groups.Count = 4)
                                ms.Groups["loc"].Value, (ms.Groups["left"].Value, ms.Groups["right"].Value)
                            )
    Map data

let directionsReader (directtions: char seq) =
    seq {
        while true do
            yield! directtions
    }

// Like Seq.takeWhile but includes the input element
// that first fails the predicate in the output
let takeWHileInclusive predicate (input: 't seq) =
    seq {
        let enum = input.GetEnumerator()
        let mutable finished = false
        while not finished && enum.MoveNext() do
            let c = enum.Current
            yield c
            if not (predicate c) then
                finished <- true
    }

// Greatest Common Factor
let gcf left right=
    // Euclid's algorithm.
    // TODO refactor to recursive to avoid the mutation
    let mutable a = left
    let mutable b = right
    while b <> 0L do
        let t = b
        b <- a % b
        a <- t
    a

// Least Common Multiple
let lcm left right = (left * right) / (gcf left right)

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn ""
    printfn $"Day 8 Part 2"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    let inputText = System.IO.File.ReadAllLines(filename)
    assert (inputText.Length >= 3)

    let sw = System.Diagnostics.Stopwatch.StartNew ()

    let directionsInput = inputText[0]
    let map = makeMap (inputText |> Seq.skip 2)
    let directions = directionsReader directionsInput

    let startingPoints
        = map.Keys
          |> Seq.where (fun k -> k.EndsWith("A"))
          |> Seq.toArray
    printfn $"Starts %A{startingPoints}"
    let count
        = startingPoints
          |> Seq.map (fun start ->
                let path
                    = directions
                        |> Seq.scan (fun position dir ->
                                    let opts = map[position]
                                    match dir with
                                    | 'L' -> fst opts
                                    | 'R' -> snd opts
                                    | _ -> failwith $"Unexpected directtion '{dir}'"
                                 ) start
                        |> Seq.mapi (fun idx pos -> (idx, pos))
                        |> takeWHileInclusive (fun (_,pos) -> not (pos.EndsWith("Z")))
                        |> Seq.last
                let pathCount = fst path
                printfn $"Start {start}: {pathCount} steps"
                pathCount
          )
          |> Seq.fold (fun a b -> lcm a b) 1

    printfn $"Final count = {count:``#,#``} ({count})"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    9
