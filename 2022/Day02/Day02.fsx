#time "on"

open System.Diagnostics;
open System.IO

let calcOneRound (input: string) =
    let moveValue inp =
        match inp with
        | "X" -> 1
        | "Y" -> 2
        | "Z" -> 3
        | _ -> raise(UnreachableException())
    let roundValue op me =
        match (op, me) with
        | ("A", "X") -> 3
        | ("A", "Y") -> 6
        | ("A", "Z") -> 0
        | ("B", "X") -> 0
        | ("B", "Y") -> 3
        | ("B", "Z") -> 6
        | ("C", "X") -> 6
        | ("C", "Y") -> 0
        | ("C", "Z") -> 3
        | _ -> raise(UnreachableException())

    let split = input.Split(" ")
    let (oponent, me) = (split[0], split[1])
    moveValue me + roundValue oponent me


printfn "Result = %d" (File.ReadLines("./Day02.txt")
                        |> Seq.map (fun l -> calcOneRound l)
                        |> Seq.sum)

(*
for l in File.ReadLines("./Day02.txt") do
    let res = calcOneRound l
    printfn "Line %s -> %d" l res
*)
