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

let visibleInDirection (startPos: int*int) (direction: int*int) =
    let startHeight = getHeight startPos

    let rec innerSearch (prevPos: int*int) =
        let thisPos = (fst prevPos + fst direction, snd prevPos + snd direction)
        let thisHeight = getHeight thisPos
        //printfn "  Comparing %A to %A is %d to %d" startPos thisPos startHeight thisHeight
        if thisHeight = -1 then
            true
        else if startHeight > thisHeight then
            innerSearch thisPos
        else
            false

    innerSearch startPos

let treeIsVisible (pos: int*int) =
    (visibleInDirection pos (0, -1))
        || (visibleInDirection pos (+1, 0))
        || (visibleInDirection pos (0, +1))
        || (visibleInDirection pos (-1, 0))

// printfn "Result for (1, 1): %b" (treeIsVisible (1, 1))
// printfn "Result for (2, 1): %b" (treeIsVisible (2, 1))
// printfn "Result for (3, 1): %b" (treeIsVisible (3, 1))
// printfn "Result for (2, 2): %b" (treeIsVisible (2, 2))
// printfn "Result for (3, 3): %b" (treeIsVisible (3, 3))
// printfn "Result for (1, 2): %b" (treeIsVisible (1, 2))

let total = seq { 0..maxSouth }
            |> Seq.collect (fun y -> seq {0..maxEast} |> Seq.map (fun x -> (x, y)))
            |> Seq.map (fun pos ->
                            let v =  treeIsVisible pos
                            //printfn "Pos %A: %b" pos v
                            v
                       )
            |> Seq.map (function | true -> 1 | false -> 0)
            |> Seq.sum
printfn "%d trees are visible" total
