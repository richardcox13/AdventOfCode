//
// Day01 Part1
//
// Usage
//  dotnet fsi .\Day01_Part1.fsx <input-file>
//

open System
open System.IO

let digits = [| '9'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]

let filename = fsi.CommandLineArgs[1]

let textReader = File.ReadLines(filename)

let numbers = textReader
                |> Seq.map (fun l ->
                                let c1 = l[l.IndexOfAny(digits)]
                                let c2 = l[l.LastIndexOfAny(digits)]
                                Int32.Parse(sprintf "%c%c" c1 c2)
                            )

// for (i, c) in (numbers |> Seq.mapi (fun i c -> (i, c))) do
//     printfn $"{i}: {c}"

printfn ""
printfn $"Result: {numbers |> Seq.sum}"
