#time "on"

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

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

let input = testInput // Or read the file

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



let updateTailForNewHeadPosition head tail =
    let (hX, hY) = head
    let (tX, tY) = tail
    let offsetX = hX - tX
    let offsetY = hY - tY
    if abs offsetX <= 1 && abs offsetY <= 1 then
        // Overlapping, or next to each other: don't mopve
        tail
    else if offsetX = 0 then
        // Is a gap but aliigned up/down
        assert (abs offsetY = 2)
        let newY = tY + (if offsetY > 0 then -1 else +1)
        (tX, newY)
    else if offsetY = 0 then
        // Is a gap but aliignedleft/right
        assert (abs offsetX = 2)
        let newX = tX + (if offsetX > 0 then -1 else +1)
        (newX, tY)
    else
        raise(UnreachableException())

let tailPositions = HashSet<int*int>()


let updatePositionWithInput oldState movement =
    let (moves, direction) = movement

    let rec updatePos oldHead oldTail count = 
        if count = 0 then
            (oldHead, oldTail)
        else
            let newHead = (fst oldHead + fst direction, snd oldHead + snd direction)
            let newTail = updateTailForNewHeadPosition newHead oldTail
            tailPositions.Add newTail |> ignore
            updatePos newHead newTail (count-1)

    updatePos (fst oldState) (snd oldState) moves


// (headPos, teailPos) each (x, y)
let initialState = ((0, 0), (0, 0))

let finalState = input
                    |> Seq.map parseOneMovement
                    |> Seq.fold updatePositionWithInput initialState
