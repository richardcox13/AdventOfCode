open System
open System.Collections.Generic
open AoC.Common

type PatternInpu = {
        Pattern: string[]
        StartIdx: int    // Offset. add one to get line
    }

let parseInput filename =
    let input = System.IO.File.ReadLines(filename)
    seq {
        let mutable acc = new List<string>()
        let mutable accStartLine = -1
        let mutable lineIdx = 0
        for line in input do            
            lineIdx <- lineIdx+1
            if String.IsNullOrWhiteSpace(line) then
                if acc.Count > 0 then
                    yield { Pattern = (acc |> Seq.toArray); StartIdx = accStartLine }
                    acc <- new List<string>()
                    accStartLine <- -1
            else
                acc.Add(line)
                if accStartLine = -1 then
                    accStartLine <- lineIdx
        if acc.Count > 0 then
            yield { Pattern = (acc |> Seq.toArray); StartIdx = accStartLine }
    }

let findReflectionCore (dirName: string)(idx: int) (pattern: string array) =
    let findInitialReflection () =
        seq {
            for row in 1 .. (pattern.Length-3) do
                if pattern[row] = pattern[row+1] then
                    yield row
        }

    let checkIsFullRelection reflectIdx =
        let rec inner offset = 
            if (reflectIdx = (offset-1) || reflectIdx+2+offset = pattern.Length) then
                // Reached the edge of the pattern, but does it contain at least 3 rows either side?
                //true
                if offset >= 2 then
                    true
                else
                    false
            else if pattern[reflectIdx-offset] = pattern[reflectIdx+1+offset] then
                inner (offset+1)
            else
                false
        inner 0

    let reflection
        = findInitialReflection ()
          |> Seq.where (fun idx -> checkIsFullRelection idx)
          |> Seq.toArray
    
    reflection |> Seq.ofArray

let findHorizonalReflection (idx: int) (pattern: string array) =
    let reflection = findReflectionCore "horizontal" idx pattern |> Seq.toArray
    if reflection.Length > 0 then
        printf "; has horizontal reflections after row indexes %s" (reflection |> Seq.map _.ToString() |> String.concat ", ")
    else
        printf $"; has no horizontal reflection"

    reflection


let findVerticalReflection (idx:int) (pattern:string array) =
    let invertPattern (map: string[]) =
        Array.init (map[0].Length)
                (fun col ->
                                seq {
                                    for row in 0 .. (map.Length-1) -> map[row][col]
                                } |> String.Concat
                           )

    let reflections = invertPattern pattern |> findReflectionCore "vertical" idx |> Seq.toArray

    if reflections.Length > 0 then
        printf "has vertical reflection after column idx %s" (reflections |> Seq.map _.ToString() |> String.concat ", ")
    else
        printf $"has no vertical reflection"

    reflections

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 13 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    use diag = Utility.GetTracker ()

    let patterns = parseInput filename |> Seq.toArray
    printfn $"There are {patterns.Length} patterns"

    let result
        = patterns
          //|> Seq.take 1 (* ***** DEBUG ***** *)
          |> Seq.mapi (fun idx pt ->
                printf $"Looking at pattern #{idx} (started at line {pt.StartIdx}) "
                //let vr = findVerticalReflection idx pt.Pattern
                //if vr.Length > 0 then
                //    printfn ""
                //    vr |> Seq.map (fun x -> x+1)
                //else
                //    let hr = findHorizonalReflection idx pt.Pattern
                //    printfn ""
                //    if hr.Length > 0 then
                //        hr |> Seq.map (fun x -> 100 * (x+1))
                //    else
                //        [||]
                let vr = findVerticalReflection idx pt.Pattern |> Seq.map (fun x -> x+1)
                let hr = findHorizonalReflection idx pt.Pattern |> Seq.map (fun x -> 100 * (x+1))
                printfn ""
                [| vr; hr |] |> Seq.concat
            )
          |> Seq.concat
          |> Seq.sum

    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
