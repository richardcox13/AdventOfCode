open System
open AoC.Common


let showSomeInputStats (input: string seq) =
    let sizes
        = input
          |> Seq.map (fun l ->
                                let ss = l.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                                assert (ss.Length = 2)
                                let (map,groups) = (ss[0],ss[1])
                                let groupCount = groups.Split(',') |> Array.length
                                let unknownCount
                                    = map
                                      |> Seq.groupBy (fun x -> x)
                                      |> Seq.where (fun (key, _) -> key = '?')
                                      |> Seq.map (fun (_, cc) -> cc |> Seq.length)
                                      |> Seq.head
                                map.Length, groupCount, unknownCount
                             )
          |> Seq.toArray
    let longestMap = sizes |> Seq.map (fun (x,_,_) -> x) |> Seq.max
    let mostGroups = sizes |> Seq.map (fun (_,x,_) -> x) |> Seq.max
    let mostUnknowns = sizes |> Seq.map (fun (_,_,x) -> x) |> Seq.max

    printfn $"Longest map: {longestMap}; most groups = {mostGroups}; most inknowns = {mostUnknowns}"
    ()

let getPeriodHashCombinations len  =
    ArgumentOutOfRangeException.ThrowIfNegativeOrZero(len)
    let rec inner prefix len =
        if len = 2 then
            [| prefix+"##"; prefix+"#."; prefix+".#"; prefix+".." |] |> Seq.ofArray
        else
            seq {
                let p = prefix + "#"
                yield! inner p (len-1)
                let p = prefix + "."
                yield! inner p (len-1)
            }

    if len = 1 then
        [| "#"; "." |] |> Seq.ofArray
    else
        inner "" len

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 12 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    let input = System.IO.File.ReadLines(filename)
    use diag = Utility.GetTracker ()

    showSomeInputStats input

    // Test combinations generation

    let js (a: string seq) =
        a |> Seq.map (fun s -> $"\"{s}\"") |> String.concat ", "

    printfn $"Length = 1: {js (getPeriodHashCombinations 1)}"
    printfn $"Length = 2: {js (getPeriodHashCombinations 2)}"
    printfn $"Length = 3: {js (getPeriodHashCombinations 3)}"
    printfn $"Length = 4: {js (getPeriodHashCombinations 4)}"
    printfn $"Length = 5: {js (getPeriodHashCombinations 5)}"


    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
