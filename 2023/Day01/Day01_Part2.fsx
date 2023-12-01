//
// Day01 Part2
//
// Usage
//  dotnet fsi .\Day01_Paet2.fsx <input-file>
//

open System
open System.IO
open System.Text.RegularExpressions


let mapMatchToDigit s =
    match s with
    | "one" -> '1'
    | "two" -> '2'
    | "three" -> '3'
    | "four" -> '4'
    | "five" -> '5'
    | "six" -> '6'
    | "seven" -> '7'
    | "eight" -> '8'
    | "nine" -> '9'
    |  s -> s[0]

let matcher = new Regex(@"\d|one|two|three|four|five|six|seven|eight|nine",
                       RegexOptions.IgnoreCase)

let filename = fsi.CommandLineArgs[1]

let textReader = File.ReadLines(filename)

let numbers = textReader
                |> Seq.map (fun l ->
                                let ms = matcher.Matches(l)

                                let m1 = ms |> Seq.head
                                let m2 = ms |> Seq.last

                                let c1 = mapMatchToDigit (m1.Value)
                                let c2 = mapMatchToDigit (m2.Value)

                                Int32.Parse(sprintf "%c%c" c1 c2)                                
                            )

for (i, c) in (numbers |> Seq.mapi (fun i c -> (i+1, c))) do
    printf $"{i}: {c} || "

printfn ""
printfn $"Result: {numbers |> Seq.sum}"
