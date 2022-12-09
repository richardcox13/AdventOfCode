#time "on"

open System.Diagnostics;
open System.IO
open System.Text.RegularExpressions

let testInputs = [|
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
    "bvwbjplbgvbhsrlpgdmjqwftvncz";
    "nppdvjthqldpwncqszvftbrmjlhg";
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
|]

let findStartBlock (input: string) =
    input |> Seq.windowed 4
          |> Seq.skipWhile (fun inp -> inp |> Array.distinct |> Array.length < 4)
          |> Seq.head
          |> Seq.map (fun c -> string c)
          |> String.concat ""

for inp in testInputs do
    let startSeq = findStartBlock inp
    let offset = inp.IndexOf(startSeq)
    printfn "Test %s -> %s offset = %d" inp startSeq (offset+4)

let inp = File.ReadAllText("Day06.txt")
let startSeq = findStartBlock inp
let offset = inp.IndexOf(startSeq)
printfn "Test %s -> %s offset = %d" inp startSeq (offset+4)
