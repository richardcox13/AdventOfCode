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

let findStartBlock (input: string)  seqLen=
    input |> Seq.windowed seqLen
          |> Seq.skipWhile (fun inp -> inp |> Array.distinct |> Array.length < seqLen)
          |> Seq.head
          |> Seq.map string
          |> String.concat ""


let processInput seqLen =
    // for inp in testInputs do
    //     let startSeq = findStartBlock inp seqLen
    //     let offset = inp.IndexOf(startSeq)
    //     printfn "Test %s -> %s offset = %d" inp startSeq (offset+seqLen)

    let inp = File.ReadAllText("Day06.txt")
    let startSeq = findStartBlock inp seqLen
    let offset = inp.IndexOf(startSeq) 
    printfn "Test %s... -> %s offset = %d" (inp |> Seq.take 25 |> Seq.map string |> String.concat "") startSeq (offset+seqLen)


printf "*** Part One ***"
processInput 4

printf "*** Part Two ***"
processInput 14
