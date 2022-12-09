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
                    |> Seq.chunkBySize 3
                    |> Seq.map (fun inp ->
                                assert (inp.Length = 3)
                                let setA = Set(inp[0])
                                let setB = Set(inp[1])
                                let setC = Set(inp[2])
                                let res = setA |> Set.intersect setB |> Set.intersect setC
                                assert (res |> Seq.length = 1)
                                res |> Seq.head
                               )
                    |> Seq.map (fun c ->
                                match c with
                                | a when a >= 'A' && a <= 'Z' -> int c - int '@' + 26
                                | a when a >= 'a' && a <= 'z' -> int c - int '`'
                                | _ -> raise(UnreachableException())
                               )
                    |> Seq.sum

printfn "Resultg = %A" res
