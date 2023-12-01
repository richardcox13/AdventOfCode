#time "on"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let testInput = [|
    "Sabqponm";
    "abcryxxl";
    "accszExk";
    "acctuvwj";
    "abdefghi";
|]


let input = testInput;

let rowLen = input[0].Length
let colLen = input.Length

type Position = { Column: int; Row: int }
                member this.Add p = { Column = this.Column + p.Column; Row = this.Row + p.Row }
                override this.ToString () = sprintf "(%d,%d)" this.Column this.Row


let findNode (letter: char) =
    let findInRow r =
        let row = input[r]
        let pos = row.IndexOf(letter)
        if pos = -1 then
            None
        else
            Some pos
    
    let rec findRow r =
        if r = colLen then
            raise(InvalidOperationException(sprintf "Failed to find %c" letter))
        else
            let c = findInRow r
            match c with
            | None -> findRow (r+1)
            | Some c -> { Column = c; Row = r }
    
    findRow 0

let start = findNode 'S'
let finish = findNode 'E'

let offsetPosirtion (p: Position) direction =
    let getDirection dir =
        match dir with
        // up
        | 1 -> { Column = 0; Row = -1 }
        // right
        | 2 -> { Column = +1; Row = 0 }
        // down
        | 3 -> { Column = 0; Row = +1 }
        // left
        | 4 -> { Column = +1; Row = 0 }
        | _ -> raise(UnreachableException("Invalid direction"))
    
    p.Add (getDirection direction)

let getChar p =
    let r = p.Row
    let c = p.Column
    if r < 0 || r > (colLen-1) then
        ' '
    else if c < 0 || c > (rowLen-1) then
        ' '
    else
        input[r][c]

let charDiff thisChar nextChar =
    let normalise c =
        if c = 'S' then
            int 'a'
        else if c = 'E' then
            int 'z'
        else
            int c
    normalise nextChar - normalise thisChar

let validMove thisChar nextChar =
    if nextChar = ' ' then
        false
    else
        let d = charDiff thisChar nextChar
        if d > 1 then
            false
        else
            true

let rec findPath currentPos history: Position list option =
    let prefix = String(' ', List.length history)
    printfn "%sCurrent Position = %s (history len = %d)" prefix (currentPos.ToString()) (List.length history)
    let newPath = currentPos :: history
    let thisChar = getChar currentPos
    let mutable result: Position list option  = None
    for dir in seq { 1..4 } |> Seq.takeWhile (fun _ -> result.IsNone) do
        let nextPos = offsetPosirtion currentPos dir
        printf "%s  Trying move %s to %s: " prefix (currentPos.ToString()) (nextPos.ToString())
        let nextChar = getChar nextPos
        if validMove thisChar nextChar then
            if nextPos = finish then
                printfn "It is the finish!!"
                // We're done!
                let fullPath = nextPos :: newPath
                result <- Some fullPath
            else if not (List.contains nextPos newPath) then
                printfn "Trying this position"
                let res = findPath nextPos newPath
                if res.IsSome then
                    result <- res
            else
                printfn "Been here already"
        else
            printfn "Not a valid move"
    if result.IsNone then
        printfn "%sFallback" prefix
    result

let fullPath = findPath start []

if fullPath.IsNone then
    printfn "FAILED to find path"
else
    printfn "Took %d steps" (fullPath.Value |> List.length)
