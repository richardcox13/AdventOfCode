open System
open System.Text.RegularExpressions

let makeMap (input: string seq) =
    let data
        = input
          |> Seq.map (fun l ->
                                let ms = Regex.Match(l, @"(?<loc>\p{Lu}{3}) = \((?<left>\p{Lu}{3}), (?<right>\p{Lu}{3})\)")
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
    let filename = args[0]
    printfn $"Input file {filename}"
    let inputText = System.IO.File.ReadAllLines(filename)
    assert (inputText.Length >= 3)

    let sw = System.Diagnostics.Stopwatch.StartNew ()

    let directionsInput = inputText[0]
    let map = makeMap (inputText |> Seq.skip 2)
    let directions = directionsReader directionsInput

    let path
        = directions
          |> Seq.scan (fun position dir ->
                                let opts = map[position]
                                match dir with
                                | 'L' -> fst opts
                                | 'R' -> snd opts
                                | _ -> failwith $"Unexpected directtion '{dir}'"
                             ) "AAA"
           |> takeWHileInclusive (fun pos -> pos <> "ZZZ")
           |> Seq.mapi (fun idx pos -> (idx, pos))
           |> Seq.toArray
    
    for (i, p) in path do
        printfn $"#{i}: {p}"

    let count = fst (path |> Array.last)

    printfn $"Count = {count:``#,#``} ({count})"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    9
