//
// Day01 Part2
//
// Usage
//  dotnet fsi .\Day01_Part2.fsx <input-file>
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
    | "zero" -> '0'
    | _ -> failwith $"cannot map \"{s}\""

let digits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]
let digitWords = [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "zero" |]

let findFirstDigitWord (input:string) =
    let f = digitWords
                |> Seq.map (fun word ->
                                let idx = input.IndexOf(word)
                                word, idx
                            )
                |> Seq.where (fun (_, idx) -> idx <> -1)
                |> Seq.toArray
    if not (f |> Seq.isEmpty) then
        let (digit, idx) =  f |> Seq.map (fun (word, idx) -> (mapMatchToDigit word), idx)
                              |> Seq.minBy (fun (_, idx) -> idx)
        (digit, idx)
    else
        ('~', Int32.MaxValue)

let findLastDigitWord (input:string) =
    let f = digitWords
                |> Seq.map (fun word ->
                                let idx = input.LastIndexOf(word)
                                word, idx
                            )
                |> Seq.where (fun (_, idx) -> idx <> -1)
                |> Seq.toArray
    if not (f |> Seq.isEmpty) then
        let (digit, idx) =  f |> Seq.map (fun (word, idx) -> (mapMatchToDigit word), idx)
                              |> Seq.maxBy (fun (_, idx) -> idx)
        (digit, idx)
    else
        ('~', -1)


let matcher = new Regex(@"one|two|three|four|five|six|seven|eight|nine|zero",
                       RegexOptions.IgnoreCase)

let filename = fsi.CommandLineArgs[1]
let textReader = File.ReadLines(filename)
let numbers = textReader
                |> Seq.mapi (fun idx l ->
                                let (firstWordChar, firstIdx) = findFirstDigitWord l
                                let digitIdx = l.IndexOfAny(digits)
                                let firstChar = if digitIdx < firstIdx then l[digitIdx] else firstWordChar

                                let (lastWordChar, lastIdx) = findLastDigitWord l
                                let digitIdx = l.LastIndexOfAny(digits)
                                let lastChar = if digitIdx > lastIdx then l[digitIdx] else lastWordChar

                                let value = Int32.Parse(sprintf "%c%c" firstChar lastChar)
                                value
                            )

                |> Seq.toArray

printfn ""
printfn $"Result: {numbers |> Seq.sum}"
