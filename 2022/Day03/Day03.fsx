#time "on"

open System.Diagnostics;
open System.IO

let input = [|
    "JrwpWtwJgWrhcsFMMfFFhFp";
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
    "PmmdzqPrVvPwwTWBwg";
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";
    "ttgJtRGJQctTZtZT";
    "CrZsJsPPZsGzwwsLwLmpwMDw";
|]

let res = File.ReadLines("./Day03.txt")
                    |> Seq.map (fun inp -> 
                                    let len = inp.Length
                                    let leftStr = inp[0..(len/2 - 1)]
                                    let left = Set(leftStr)
                                    let rightStr = inp[(len/2)..(len-1)]
                                    let right = Set(rightStr)
                                    Set.intersect left right |> Seq.head)
                    |> Seq.map (fun c ->
                                match c with
                                | a when a >= 'A' && a <= 'Z' -> int c - int '@' + 26
                                | a when a >= 'a' && a <= 'z' -> int c - int '`'
                                | _ -> raise(UnreachableException())
                               )
                    |> Seq.sum

printfn "Resultg = %A" res
