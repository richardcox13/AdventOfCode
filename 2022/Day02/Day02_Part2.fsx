#time "on"

open System.Diagnostics;
open System.IO

type PossibleResults = { Loose: int ; Draw: int; Win: int }
let resultMap = Map([|
    // Rock -> Loose: Scissors, Draw: Rock, Win: Paper
    ("A", {Loose = 3; Draw = 4; Win = 8});
    // Paper -> Loose: Rock, Draw: Paper, Win: Scissors
    ("B", {Loose = 1; Draw = 5; Win = 9});
    // Scissors -> Loose: Paper, Draw: Scissors, Win: Rock
    ("C", {Loose = 2; Draw = 6; Win = 7});
|])


let calcOneRound (input: string) =
    let getResult possibilities selection =
        match selection with
        | "X" -> possibilities.Loose
        | "Y" -> possibilities.Draw
        | "Z" -> possibilities.Win
        | _ -> raise(UnreachableException())

    let split = input.Split(" ")
    let (oponent, me) = (split[0], split[1])
    let poss = resultMap[oponent]
    getResult poss me


printfn "Result = %d" (File.ReadLines("./Day02.txt")
                        |> Seq.map (fun l -> calcOneRound l)
                        |> Seq.sum)

(*
for l in File.ReadLines("./Day02.txt") do
    let res = calcOneRound l
    printfn "Line %s -> %d" l res
*)
