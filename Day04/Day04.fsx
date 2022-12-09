#time "on"

open System.Diagnostics;
open System.IO
open System.Text.RegularExpressions

let input = [|
    "2-4,6-8";
    "2-3,4-5";
    "5-7,7-9";
    "2-8,3-7";
    "6-6,4-6";
    "2-6,4-8";
|]

let matcher = Regex("(?<a>\d+)-(?<b>\d+),(?<c>\d+)-(?<d>\d+)")

let res = File.ReadLines("./Day04.txt")
                    |> Seq.map (fun inp -> 
                                    let m = matcher.Match(inp)
                                    (int m.Groups["a"].Value, int m.Groups["b"].Value, int m.Groups["c"].Value, int m.Groups["d"].Value)
                               )
                    |> Seq.map (fun (a, b, c, d) ->
                                        let isSubset = if (a <= c && d <= b) || (c <= a && b <= d) then 1 else 0
                                        let isOveralp = if (a <= d && b >= c) then 1 else 0
                                        (isSubset, isOveralp)
                                  )
                    |> Seq.fold (fun (tSubset, tOverlap) (subset, overlap) -> (tSubset+subset, tOverlap+overlap)) (0, 0)
let (subsets, overlaps) = res
printfn "Subsets (part 1) = %d; Overlaps (part 2) = %d" subsets overlaps
