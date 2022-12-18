#time "on"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let testInput = [|
    "[1,1,3,1,1]";
    "[1,1,5,1,1]";
    "";
    "[[1],[2,3,4]]";
    "[[1],4]";
    "";
    "[9]";
    "[[8,7,6]]";
    "";
    "[[4,4],4,4]";
    "[[4,4],4,4,4]";
    "";
    "[7,7,7,7]";
    "[7,7,7]";
    "";
    "[]";
    "[3]";
    "";
    "[[[]]]";
    "[[]]";
    "";
    "[1,[2,[3,[4,[5,6,7]]]],8,9]";
    "[1,[2,[3,[4,[5,6,0]]]],8,9]";
|]

type Packet = 
        | Value of int
        // Can have a zero length list...
        | Sequence of Packet list
        override this.ToString() =
                match this with
                | Value x -> x.ToString()
                | Sequence xs ->
                    let content = xs |> Seq.map (fun p -> p.ToString()) |> String.concat ", "
                    "[" + content + "]"

let (|PackeStart|_|) (input: string) =
    if input.Length > 0 && input[0] = '[' then
        Some (input.Substring(1))
    else
        None

let (|PacketEnd|_|) (input: string) =
    if input.Length > 0 && input[0] = ']' then
        Some (input.Substring(1))
    else
        None

let matchNumber = Regex("^(\\d+),?")

let (|Number|_|) (input: string) =
    if input.Length = 0 then
        None
    else
        let m = matchNumber.Match(input)
        if not m.Success then
            None
        else
            let g = m.Groups[1].Value
            // Use whole match for the skip, so will incoude the comma
            Some (int g, input.Substring(m.Value.Length))

let parsePacket (input: string) =
    let rec doParse input (currentSeq: Packet list) =
        printfn "Matching: %s" input
        match input with
        | "" ->
            printfn "Empty input: current seq: %s" (currentSeq |> Seq.map (fun p -> p.ToString()) |> String.concat "//")
            (currentSeq, "")
        | PackeStart rest ->
            let (innerSeq, rr) = doParse rest []
            doParse rr (currentSeq @ [Sequence(innerSeq)])
        | PacketEnd rest ->
             (currentSeq, rest)
        | Number (n, rest) ->
            doParse rest (currentSeq @ [Value(n)])
        | _ -> raise(UnreachableException(sprintf "Failed to match \"%s\"" input))
    let res = doParse input []
    fst res

let pp = Sequence([Value(10); Value(13)])
printfn "pp: %s" (pp.ToString())

let x = parsePacket "[1,2]"
printfn ">>%s<<" (x.ToString())

