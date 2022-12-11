#time "on"

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let testInput = [|
    "30373";
    "25512";
    "65332";
    "33549";
    "35390";
|]


let input = testInput // Or read from Day08.txt

let maxEast = input[0].Length-1
let maxSouth = input.Length-1

let getHeight (startPos: int*int) =
    let (x, y) = startPos
    if x < 0 || y < 0 || x > maxEast || y > maxSouth then
        -1
    else
        // '0' with ASCII 
        int (input[y][x]) - 48

let rec visibleInDirection (startPos: int*int) (direction: int*int) =
    let this = getHeight startPos
    let nextPos = (fst startPos + fst direction, snd startPos + snd direction)
    let next = getHeight nextPos
    printfn "  Comparing %A to %A is %d to %d" startPos nextPos this next
    if next = -1 then
        true
    else if this > next then
        visibleInDirection nextPos direction
    else
        false

printfn "(1, 1) going north: %b" (visibleInDirection (1,1) (0, -1))
printfn "(1, 1) going east: %b" (visibleInDirection (1,1) (+1, 0))
printfn "(1, 1) going south: %b" (visibleInDirection (1,1) (0, +1))
printfn "(1, 1) going west: %b" (visibleInDirection (1,1) (-1, 0))

