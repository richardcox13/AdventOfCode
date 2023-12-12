open System
open System.Text.RegularExpressions
open AoC.Common

type OneInput = {
        Map: string
        Groups: int array
    }

let countChar (char: char) (str: string) =
    let rec inner (pos: int) count =
        let idx = str.IndexOf(char, pos)
        if idx = -1 then
            count
        else
            inner (idx+1) (count+1)
    inner 0 0

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

let parseInput (input: string seq) =
    input
        |> Seq.map (fun line ->
            let ss = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            assert (ss.Length = 2)
            let (map, groupsStr) = (ss[0],ss[1])
            let groups = groupsStr.Split(",") 
                              |> Seq.map (fun n -> Int32.Parse(n.Trim()))
                              |> Seq.toArray
            { Map = map; Groups = groups }
          )

let applyReplacements map (replacements: string) =
    let mutable replaceIdx = -1
    Regex.Replace(map, @"\?",
                    fun _ -> 
                                replaceIdx <- replaceIdx + 1
                                string (replacements[replaceIdx])
                 )

let validateReplacement (groups: int array) target =
    // Find all the matches of '#' ... needs to match in length
    // and number of the groups in the input
    let ms = Regex.Matches (target, @"#+")
    if ms.Count <> groups.Length then
        false
    else
        (ms, groups)
            ||> Seq.zip
            |> Seq.forall (fun (m, g) -> m.Value.Length = g)

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
    //let js (a: string seq) =
    //    a |> Seq.map (fun s -> $"\"{s}\"") |> String.concat ", "
    //printfn $"Length = 1: {js (getPeriodHashCombinations 1)}"
    //printfn $"Length = 2: {js (getPeriodHashCombinations 2)}"
    //printfn $"Length = 3: {js (getPeriodHashCombinations 3)}"
    //printfn $"Length = 4: {js (getPeriodHashCombinations 4)}"
    //printfn $"Length = 5: {js (getPeriodHashCombinations 5)}"

    // Quick test of substitution
    //printfn ""
    //let r = applyReplacements "?.?#??" "1234"
    //printfn $"substitute \"?.?#??\" with \"1234\": \"{r}\""

    // Quick check of valudation
    //let checkValidation groups map =
    //    let groupToString grp = grp |> Seq.map (fun x -> $"{x}") |> String.concat ","
    //    let res = validateReplacement groups map
    //    printfn $"vaidate [| {groupToString groups} |] vs \"{map}\": {res}"
    //checkValidation ([| 1; 1 |]) ".#.#."
    //checkValidation ([| 1; 1 |]) ".##.."
    //checkValidation ([| 2 |]) ".##.."
    //checkValidation ([| 2;3;5 |]) "##.###....#####"

    let allInput = parseInput input

    let result
        = allInput
          //|> Seq.skip 1 |> Seq.take 1 (* Testing, 2nd example is more interesting *)
          |> Seq.map (fun inp ->
                let subCount = countChar '?' inp.Map

                getPeriodHashCombinations subCount
                    |> Seq.map (fun sub -> applyReplacements inp.Map sub)
                    |> Seq.where (fun sub -> validateReplacement inp.Groups sub)
                    |> Seq.length
            )
          |> Seq.sum

    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
