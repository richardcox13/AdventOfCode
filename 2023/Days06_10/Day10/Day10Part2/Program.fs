open System
open System.Text.RegularExpressions
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

type PipelineStep = {
    CurPos: Position
    CurChar: char    
    CurDir: Cardinal
    Dist: int
    NewPos: Position
    NewChar: char
    NewDir: Cardinal
}

type AnnotatedTile = {
        Char: char
        IsPipeline: bool
    }

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
        yield { CurPos = startPos; CurChar = maze[startPos.Row][startPos.Col];  CurDir = firstDir;
                Dist = 0;
                NewPos = curPos; NewChar = curChar; NewDir = curDir }
        let mutable dist = 1
        while curChar <> 'S' do
            let newDir= getDirectionFromChar curDir curChar
            let newPos = getPositionInDirecttion maze curPos newDir
            let newChar = maze[newPos.Row][newPos.Col]
            yield { CurPos = curPos; CurChar = curChar;  CurDir = curDir;
                    Dist = dist;
                    NewPos = newPos; NewChar = newChar; NewDir = newDir }
            dist <- dist+1
            curPos <- newPos
            curChar <- newChar
            curDir <- newDir
    }

let buildmarkedMaze (maze: string[]) path =
    let markedMaze
        = Array.init maze.Length
            (fun r ->
                            Array.init maze[0].Length (fun c ->
                                { Char = maze[r][c]; IsPipeline = false }
                            )
                       )

    path |> Seq.iter (fun p ->
                                let pos = p.CurPos;
                                let c = p.CurChar
                                markedMaze[pos.Row][pos.Col] <-  { Char = c; IsPipeline = true }
                            )
    markedMaze

let countInternalTiles (maze: AnnotatedTile array array) =
    let tileIsInterior row col =
        let slice
            = maze[row][0..col]
              |> Array.where (fun t -> t.IsPipeline)
              |> Array.map (fun t -> t.Char)
        let s = new String(slice)
        if s.Length < 1 then
            false
        else
            let ms = Regex.Matches(s, @"F-*J|L-*7|\|")
            ms.Count % 2 = 1
    let mutable count = 0
    for row in seq { 1 .. (maze.Length-2) } do
        for col in seq { 1 .. (maze[0].Length-2)} do
            let t = maze[row][col]
            if not (t.IsPipeline) && (tileIsInterior row col) then
                count <- count+1
    count

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
    
    let pipePath
        = followPipe maze startPosition
          |> Seq.toArray

    let pipelineLength
        = pipePath
          |> Seq.mapi (fun idx r ->
                                printfn $"#{idx} current pos={r.CurPos} ('{r.CurChar}'), dir={r.CurDir}; dist={r.Dist} --> {r.NewPos} ('{r.NewChar}'), dir={r.NewDir}"
                                r.Dist
                              )
          |> Seq.last

    let markedMaze = buildmarkedMaze maze pipePath
    let internalTileCount = countInternalTiles markedMaze

    printfn ""
    printfn $"Internal tile count = {internalTileCount:``#,0``} ({internalTileCount})"
    printfn ""
    0
