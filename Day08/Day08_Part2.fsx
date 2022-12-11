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


let input = File.ReadAllLines("./Day08.txt") // Or read testInput

let maxEast = input[0].Length-1
let maxSouth = input.Length-1

let getHeight (startPos: int*int) =
    let (x, y) = startPos
    if x < 0 || y < 0 || x > maxEast || y > maxSouth then
        -1
    else
        // '0' with ASCII 
        int (input[y][x]) - 48

let scenicScoreInOneDirection startPos direction =
    let startHeight = getHeight startPos

    let rec innerSearch prevPos acc =
        let thisPos = (fst prevPos + fst direction, snd prevPos + snd direction)
        let thisHeight = getHeight thisPos
        //printfn "  Comparing %A to %A is %d to %d (current acc = %d)" startPos thisPos startHeight thisHeight acc
        if thisHeight = -1 then
            acc
        else if startHeight > thisHeight then
            innerSearch thisPos (acc+1)
        else
            acc+1
    let x = innerSearch startPos 0
    //printfn "  Direction %A from %A scores %d" direction startPos x
    x

// printfn "Score north: %d" (scenicScoreInOneDirection (2, 1) (0, -1))
// printfn "Score east: %d" (scenicScoreInOneDirection (2, 1) (+1, 0))
// printfn "Score south: %d" (scenicScoreInOneDirection (2, 1) (0, +1))
// printfn "Score west: %d" (scenicScoreInOneDirection (2, 1) (-1, 0))

let scenicScore startPos =
    scenicScoreInOneDirection startPos (0, -1)
    * scenicScoreInOneDirection startPos (0, +1)
    * scenicScoreInOneDirection startPos (-1, 0)
    * scenicScoreInOneDirection startPos (+1, 0)

// printfn "Scenic score for (2, 1): %d" (scenicScore (2, 1))
// printfn "Scenic score for (2, 3): %d" (scenicScore (2, 3))
// printfn "Scenic score for (0, 4): %d" (scenicScore (0, 4))


let total = seq { 0..maxSouth }
            |> Seq.collect (fun y -> seq {0..maxEast} |> Seq.map (fun x -> (x, y)))
            |> Seq.map (fun pos ->
                            let v =  scenicScore pos
                            //printfn "Pos %A: %b" pos v
                            v
                       )
            |> Seq.max
printfn "Maximum scenic score: %d" total
