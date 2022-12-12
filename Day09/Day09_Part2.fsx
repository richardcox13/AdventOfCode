#time "on"

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

// Expected result = 1
let testInput = [|
    "R 4";
    "U 4";
    "L 3";
    "D 1";
    "R 4";
    "D 1";
    "L 5";
    "R 2";
|]

// Expected result 36
let testInput2 = [|
    "R 5";
    "U 8";
    "L 8";
    "D 3";
    "R 17";
    "D 10";
    "L 25";
    "U 20";
|]

let input = (* File.ReadAllLines("./Day09.txt") // *) testInput

let matchInput = Regex("([UDLR]) (\d+)")

let parseOneMovement inp =
    let getDirection move =
        match move with
        | "L" -> (-1, 0)
        | "R" -> (+1, 0)
        | "U" -> (0, +1)
        | "D" -> (0, -1)
        | _ -> raise(UnreachableException())

    let m = matchInput.Match(inp)
    assert (m.Success)
    let dir = m.Groups[1].Value
    let offset = getDirection dir
    let count = int (m.Groups[2].Value)
    (count, offset)

let updateKnotForPreviousKnotPosition prevKnot thisKnot =
    let sign x =
        if x > 0 then
            +1
        else if x < 0 then
            -1
        else
            0

    let (pX, pY) = prevKnot
    let (tX, tY) = thisKnot
    let offsetX = pX - tX
    let offsetY = pY - tY
    let absX = abs offsetX
    let absY = abs offsetY

    if absX <= 1 && absY <= 1 then
        // Overlapping, or next to each other: don't mopve
        //printfn "    Tail does not move, stays at (%d, %d)" (fst tail) (snd tail)
        thisKnot
    else if offsetX = 0 then
        // Is a gap but aliigned up/down
        assert (absY = 2)
        let newY = tY + (sign offsetY)
        let p = (tX, newY)
        //printfn "    Y straight move tail to (%d, %d)" (fst p) (snd p)
        p
    else if offsetY = 0 then
        // Is a gap but aliignedleft/right
        assert (absX = 2)
        let newX = tX + (sign offsetX)
        let p = (newX, tY)
        //printfn "    X straight move tail to (%d, %d)" (fst p) (snd p)
        p
    else if absX = 1 && absY = 2 then
        // Need a diagonal move (first set of cases)
        let p = (tX + offsetX, tY + offsetY/2)
        //printfn "    Move tail to (%d, %d)" (fst p) (snd p)
        p
    else if absX = 2 && absY = 1 then
        // Need a diagonal move (second set of cases)
        let p = (tX + offsetX/2, tY + offsetY)
        //printfn "    Move tail to (%d, %d)" (fst p) (snd p)
        p
    else
        let msg = sprintf "Unexpected relative head and tail: head (%d, %d), tail (%d, %d) offset (%d, %d)"
                            pX pY tX tY offsetX offsetY
        raise(UnreachableException(msg))

let tailPositions = HashSet<int*int>()

let updatePositionWithInput oldState movement =
    let (moves, direction) = movement
    //printfn "Move %d times offset (%d, %d)" moves (fst direction) (snd direction)

    let rec updatePos oldHead oldTail count = 
        if count = 0 then
            (oldHead, oldTail)
        else
            let newHead = (fst oldHead + fst direction, snd oldHead + snd direction)
            //printfn "  Moved to head (%d, %d)" (fst newHead) (snd newHead)
            let newTail = updateKnotForPreviousKnotPosition newHead oldTail
            tailPositions.Add newTail |> ignore
            updatePos newHead newTail (count-1)

    updatePos (fst oldState) (snd oldState) moves


// (headPos, teailPos) each (x, y)
let initialState = ((0, 0), (0, 0))

let finalState = input
                    |> Seq.map parseOneMovement
                    |> Seq.fold updatePositionWithInput initialState

let ((hX, hY), (tX, tY)) = finalState

printfn "Final positions: head (%d, %d), tail (%d, %d)" hX, hY, tX, tY

printfn "%d positions occupied by the tail" (tailPositions.Count)
