#time "on"

open System;
open System.Diagnostics;
open System.IO
open System.Text.RegularExpressions

(*
let input = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2  
"""
*)

let input = File.ReadAllText("./Day05.txt")

// Break up input into initial stack setup, and moved
let (setupRawStr, movesRawStr) = match input.Split("\r\n\r\n") with
                                                  | [| a; b |] -> (a, b)
                                                  | _ -> raise(UnreachableException())

// Want to extra array of arrays from the initial setup.
// Effectively need to pivot, so start with the last row to get the stack count
let setupStrs = setupRawStr.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
let setupLastStr = setupStrs |> Array.last
let stackCount = Regex.Count(setupLastStr, " \d ")
printfn "There are %d stacks" stackCount

// stacks is array of arrays
let stacks = Array.init stackCount (fun _ -> Array.Empty<char>())
// Work from the top of the stacks, putting the new element at the front of the inner
// arrays
for r in 0..(setupStrs.Length-2) do
    let row = setupStrs[r]
    for col in 0..(stackCount-1) do
        let charCol = col*4 + 1
        let c = row[charCol]
        if c <> ' ' then
            stacks[col] <- Array.insertAt 0 c stacks[col]
printfn "Starting stacks:"
for s in 0..stackCount-1 do
    printfn "  #%d: %s" (s+1) (String(stacks[s]))

let movePattern = Regex("move (?<qty>\d+) from (?<src>\d+) to (?<dst>\d+)")

let moves = movesRawStr.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
                        |> Seq.mapi (fun idx l ->
                                     let m = movePattern.Match(l)
                                     if not m.Success then
                                        raise(Exception(sprintf "Failed to match line %d: %s" idx l))
                                     (int m.Groups["qty"].Value, int m.Groups["src"].Value, int m.Groups["dst"].Value)
                                    )

let singleMove qty src dst =
    // Input is one based, but array is zero based
    let srcStack = stacks[src-1]
    let newSourceLen = srcStack.Length - qty
    stacks[src-1] <- srcStack |> Array.take newSourceLen
    stacks[dst-1] <- Array.append (stacks[dst-1]) (srcStack |> Array.skip newSourceLen)


for (q, s, d) in moves do
    //printfn "%d from %d to %d" q s d
    singleMove q s d
    // printfn "  After move stacks:"
    // for s in 0..stackCount-1 do
    //     printfn "    #%d: %s" (s+1) (String(stacks[s]))

printfn "Final stacks:"
for s in 0..stackCount-1 do
    printfn "  #%d: %s" (s+1) (String(stacks[s]))

let res = stacks |> Seq.map (fun stk -> string (stk |> Array.last)) |> String.concat ""

printfn "Result: %s" res
