//
// Day01 Part2
//
// Usage
//  dotnet fsi .\Day02_Part2.fsx <input-file>

open System
open System.IO
open System.Text.RegularExpressions

type Draw = {
    Red: int
    Green: int
    Blue: int
}

let extractDraws (draws: string seq) =
    // Separate the cubes in each draw: seq of "colour n"
    draws |> Seq.collect _.Split(',')
          // Get (Color, n)
          |> Seq.map (fun c ->
                    let xs = c.Trim().Split(' ')
                    assert (xs.Length = 2)
                    xs[1], Int32.Parse(xs[0])
          )
          // And from each build the max for each color across the draws
          |> Seq.fold (fun old (col, n) ->
                        match col with
                        | "red" -> { old with Red = Math.Max(n, old.Red)}
                        | "green" -> { old with Green = Math.Max(n, old.Green)}
                        | "blue" -> { old with Blue = Math.Max(n, old.Blue)}
                        | _ -> failwith $"Unexpected color \"{col}\""
          ) { Red = 0; Green = 0; Blue = 0}

let breakInputLine (inputLine: string) =
    // All lines start "Game "
    let input = inputLine.Substring(5)
    let gameNoStrings = input.Split(':')
    assert (gameNoStrings.Length = 2)
    let gameNo = Int32.Parse(gameNoStrings[0])
    //printf $"{gameNo}: "
    let draws = gameNoStrings[1].Split(';') |> Seq.map _.Trim()
    // for d in draws do
    //     printf $"({d})"
    gameNo, draws

// Process out input line as one game, its (gameNo, power)
let processGame (inputLine: string) =
    let (gameNo, draws) = breakInputLine inputLine
    let maxes = extractDraws draws
    //printfn $" -> (Red={maxes.Red}; Green={maxes.Green} ; Blue={maxes.Blue})"
    gameNo, (maxes.Red * maxes.Green * maxes.Blue)

let filename = fsi.CommandLineArgs[1]
let textReader = File.ReadLines(filename)

printfn ""
let resSeq = textReader |> Seq.map (fun l -> processGame l) |> Seq.toList
resSeq |> Seq.iter (fun (no, pwr) -> printf $"({no}, {pwr}) ")
printfn ""
let result = resSeq |> Seq.sumBy (fun (_, pwr) -> pwr)
printfn $"Result = {result}"
