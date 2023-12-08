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
    printfn $"Starting at {startingPoints}"
    let path
        = directions
          |> Seq.scan (fun positions dir ->
                                positions
                                    |> Seq.map (fun pos ->
                                        let opts = map[pos]
                                        match dir with
                                        | 'L' -> fst opts
                                        | 'R' -> snd opts
                                        | _ -> failwith $"Unexpected directtion '{dir}'"
                                    )
                                    |> Seq.toArray
                             ) startingPoints
           |> Seq.mapi (fun idx pos -> (idx, pos))
           |> Seq.map (fun (idx, positions) ->
                                    printfn $"#{idx}: %A{positions}"
                                    (idx, positions)
                              )
           |> takeWHileInclusive (fun (_,positions) -> not (positions |> Seq.forall (fun p -> p.EndsWith("Z"))))
           |> Seq.toArray
    
    let count = fst (path |> Array.last)

    printfn $"Count = {count:``#,#``} ({count})"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    9
