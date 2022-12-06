#time "on"

open System;
open System.Diagnostics;
open System.IO
open System.Text.RegularExpressions

let input = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2  
"""

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
    printfn "  #%d: %s" s (String(stacks[s]))
