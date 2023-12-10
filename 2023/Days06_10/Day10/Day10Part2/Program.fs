open System
open AoC.Common

[<CustomEquality>][<NoComparison>]
type Position =
    { Row: int; Col: int }
    override x.ToString() = $"({x.Row}, {x.Col})"
    override x.GetHashCode() = HashCode.Combine(x.Row, x.Col)
    override left.Equals right =
        match right with
        | :? Position as r when left.Row = r.Row && left.Col = r.Col
            -> true
        | _ -> false

type Cardinal =
    | North = 1
    | East = 2
    | South = 3
    | West = 4

let findStart (maze: string[]) =
    let srch
        = maze
          |> Seq.mapi (fun idx row ->
                let pos = row |> Seq.tryFindIndex (fun c -> c = 'S')
                idx, pos
           )
          |> Seq.where (fun (_, pos) -> pos.IsSome)
          |> Seq.head
    { Row = fst srch; Col = (snd srch).Value }

let getPositionInDirecttion (maze: string[]) position direction =
    match direction with
    | Cardinal.North -> { Row = position.Row - 1; Col =position.Col }
    | Cardinal.East -> { Row = position.Row; Col = position.Col + 1 }
    | Cardinal.South -> { Row = position.Row + 1; Col = position.Col }
    | Cardinal.West -> { Row = position.Row; Col = position.Col - 1 }
    | d -> failwith $"Unexpected direction {d}"

let getCharInDirection (maze: string[]) position direction =
    let nextPos = getPositionInDirecttion maze position direction
    maze[nextPos.Row][nextPos.Col]

let tryGetCharInDirection (maze: string[]) position direction =
    let nextPos = getPositionInDirecttion maze position direction
    if nextPos.Row < 0
        || nextPos.Row > maze.Length-1
        || nextPos.Col < 0
        || nextPos.Col > maze[0].Length-1 then
        None
    else
        Some (maze[nextPos.Row][nextPos.Col])

let isValidForDirection c dir =
    match (dir, c) with
    | (Cardinal.North, '|')
    | (Cardinal.North, 'F')
    | (Cardinal.North, '7') -> true
    | (Cardinal.East, '-')
    | (Cardinal.East, 'J')
    | (Cardinal.East, '7') -> true
    | (Cardinal.South, '|')
    | (Cardinal.South, 'L')
    | (Cardinal.South, 'J') -> true
    | (Cardinal.West, '-')
    | (Cardinal.West, 'L')
    | (Cardinal.West, 'F') -> true
    | _ -> false

let findInitialDirection (maze: string[]) startPos =
    // There is no weighting here, and – given closed loop – there will be two,
    // as to which direction to prefer to start
    //let mutable startDir: Cardinal option = None
    let (charNorth, charEast, charSouth, charWest)
        = (tryGetCharInDirection maze startPos Cardinal.North),
          (tryGetCharInDirection maze startPos Cardinal.East),
          (tryGetCharInDirection maze startPos Cardinal.South),
          (tryGetCharInDirection maze startPos Cardinal.West)
    if charNorth.IsSome
        && isValidForDirection charNorth.Value Cardinal.North then
        Cardinal.North, (getPositionInDirecttion maze startPos Cardinal.North)
    else if charEast.IsSome
            && isValidForDirection charEast.Value Cardinal.East then
        Cardinal.East, (getPositionInDirecttion maze startPos Cardinal.East)
    else if charSouth.IsSome
            && isValidForDirection charSouth.Value Cardinal.South then
        Cardinal.South, (getPositionInDirecttion maze startPos Cardinal.South)
    else 
        assert (charWest.IsSome)
        if isValidForDirection charEast.Value Cardinal.West then
            Cardinal.West, (getPositionInDirecttion maze startPos Cardinal.West)
        else
            failwith $"No valid direction from {startPos}"

let getDirectionFromChar incomingDirection curChar =
    match (incomingDirection, curChar) with
    | (Cardinal.North, '|') -> Cardinal.North
    | (Cardinal.North, 'F') -> Cardinal.East
    | (Cardinal.North, '7') -> Cardinal.West
    | (Cardinal.East, '-') -> Cardinal.East
    | (Cardinal.East, 'J') -> Cardinal.North
    | (Cardinal.East, '7') -> Cardinal.South
    | (Cardinal.South, '|') -> Cardinal.South
    | (Cardinal.South, 'L') -> Cardinal.East
    | (Cardinal.South, 'J') -> Cardinal.West
    | (Cardinal.West, '-') -> Cardinal.West
    | (Cardinal.West, 'L') -> Cardinal.North
    | (Cardinal.West, 'F') -> Cardinal.South
    | _ -> failwith $"Cannot move {incomingDirection} into '{curChar}'"


let followPipe (maze: string[]) startPos =
    let (firstDir, nextPos) = findInitialDirection maze startPos
    printfn $"First move will be {firstDir} to {nextPos}"
    seq {
        let mutable curPos = getPositionInDirecttion maze startPos firstDir
        let mutable curChar = maze[curPos.Row][curPos.Col]
        let mutable curDir = firstDir
        let mutable dist = 1
        while curChar <> 'S' do
            let newDir= getDirectionFromChar curDir curChar
            let newPos = getPositionInDirecttion maze curPos newDir
            let newChar = maze[newPos.Row][newPos.Col]
            yield {| CurPos = curPos; CurChar = curChar;  CurDir = curDir;
                     Dist = dist;
                     NewPos = newPos; NewChar = newChar; NewDir = newDir |}
            dist <- dist+1
            curPos <- newPos
            curChar <- newChar
            curDir <- newDir
    }

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 10 Part 2"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    let maze= System.IO.File.ReadAllLines(filename)
    use diag = Utility.GetTracker ()

    let startPosition = findStart maze
    printfn $"Start = {startPosition}"
    
    let path = followPipe maze startPosition

    let resultDist
        = path
          |> Seq.mapi (fun idx r ->
                                printfn $"#{idx} current pos={r.CurPos} ('{r.CurChar}'), dir={r.CurDir}; dist={r.Dist} --> {r.NewPos} ('{r.NewChar}'), dir={r.NewDir}"
                                r.Dist
                             )
          |> Seq.last

    printfn ""
    printfn $"Final dist = {resultDist:``#,#``}"
    let maxAway = int (ceil ((float resultDist) / 2.0))
    printfn $"Final dist = {maxAway:``#,#``} ({maxAway})"
    printfn ""
    0
