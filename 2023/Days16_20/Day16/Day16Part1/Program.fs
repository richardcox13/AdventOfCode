﻿open System
open System.Collections.Generic
open System.IO
open AoC.Common


type Direction =
    | Up = 1uy
    | Left = 2uy
    | Down = 3uy
    | Right = 4uy

[<Struct>]
type Position =
    { Row: int16
      Col: int16 }
    override x.ToString() = $"({x.Row}, {x.Col})"

type GridCell = {
        Symbol: char
        // Already existing inbound beams
        mutable HasUpwardsBeam: bool
        mutable HasLeftwardsBeam: bool
        mutable HasDownwardsBeam: bool
        mutable HasRightwardsBeam: bool
    }

type Grid = GridCell array2d

let makeGridCell symbol
    = { Symbol = symbol
        HasUpwardsBeam = false
        HasLeftwardsBeam = false
        HasDownwardsBeam = false
        HasRightwardsBeam = false }

let buildGrid (input: string array) =
    Array2D.init input.Length input[0].Length
        (fun r c -> makeGridCell (input[r][c]))

let getCell (pos: Position) (grid: Grid) =
    grid[int pos.Row, int pos.Col]

let getCellSymbol pos grid = (getCell pos grid).Symbol

let getCellDirectionFlag dir cell =
    match dir with
    | Direction.Up -> cell.HasUpwardsBeam
    | Direction.Down -> cell.HasDownwardsBeam
    | Direction.Left -> cell.HasLeftwardsBeam
    | Direction.Right -> cell.HasRightwardsBeam
    | _  -> failwith $"Unexpected direction {dir}"

let rowCount grid = int16 (Array2D.length1 grid)
let colCount grid = int16 (Array2D.length2 grid)

let printGrid (title:string) (grid: Grid) =
    Console.Write(title)
    Console.WriteLine(':')
    let rc = rowCount grid
    let cc = colCount grid
    for r in 0s .. (rc-1s) do
        Console.Write("    ")
        for c in 0s .. (cc-1s) do
            Console.Write(getCellSymbol { Row = r; Col = c} grid)
        Console.WriteLine()
    Console.WriteLine()

let printEnergisedGrid (title:string) (grid: Grid) =
    Console.Write(title)
    Console.WriteLine(':')
    let rc = rowCount grid
    let cc = colCount grid
    for r in 0s .. (rc-1s) do
        Console.Write("    ")
        for c in 0s .. (cc-1s) do
            let c= getCell { Row = r; Col = c} grid
            let e = c.HasDownwardsBeam || c.HasLeftwardsBeam || c.HasRightwardsBeam || c.HasUpwardsBeam
            let s = if e then '#' else '.'
            Console.Write(s)
        Console.WriteLine()
    Console.WriteLine()

let moveLeft pos = { Row = pos.Row; Col = pos.Col - 1s }
let moveRight pos = { Row = pos.Row; Col = pos.Col + 1s }
let moveUp pos = { Row = pos.Row - 1s; Col = pos.Col }
let moveDown pos = { Row = pos.Row + 1s; Col = pos.Col }

let moveDir pos dir =
    let f = match dir with
            | Direction.Up -> moveUp
            | Direction.Down -> moveDown
            | Direction.Left -> moveLeft
            | Direction.Right -> moveRight
            | _  -> failwith $"Unexpected direction {dir}"
    f pos

let countEnergisedCells (grid: Grid) =
    let rc = rowCount grid
    let cc = colCount grid

    seq {
        for r in 0s .. (rc-1s) do
            for c in  0s .. (cc-1s) do
                { Row = r; Col = c }
    }
    |> Seq.map (fun p -> getCell p grid)
    |> Seq.where (fun c -> c.HasUpwardsBeam || c.HasDownwardsBeam || c.HasLeftwardsBeam || c.HasRightwardsBeam)
    |> Seq.length

let applyBeams (grid: Grid) startPos startDirection =
    let diag (msg: string) =
        //Console.Write(msg)
        ()
    let diagn(msg: string) =
        //Console.WriteLine(msg)
        () 
    let isInGrid pos =
        pos.Row >= 0s
            && pos.Row < (rowCount grid)
            && pos.Col >= 0s
            && pos.Col < (colCount grid)

    let updateCellWithBeam pos dir =
        let c = getCell pos grid
        match dir with
        | Direction.Up -> c.HasUpwardsBeam <- true
        | Direction.Down -> c.HasDownwardsBeam <- true
        | Direction.Left -> c.HasLeftwardsBeam <- true
        | Direction.Right -> c.HasRightwardsBeam <- true
        | _  -> failwith $"Unexpected direction {dir}"

    // Pos is current cell, dir inbound direction
    let rec iterate pos dir its =
        diag $"Iteration #{its}: to {pos} coming {dir}"
        if not (isInGrid pos) then
            diagn " outside grid: end of beam"
            ()
        else
            let cell = getCell pos grid
            if getCellDirectionFlag dir cell then
                diagn " this cell has already been visited in this direction: end of beam"
                ()
            else
                updateCellWithBeam pos dir
                let sym = getCellSymbol pos grid
                diag $" matching ({dir}, '{sym}')"
                match (dir, sym) with
                | (d, '.')
                        -> diagn ""
                           iterate (moveDir pos dir) dir (its+1)
                | (Direction.Up, '|')
                | (Direction.Down, '|')
                        -> diagn ""
                           iterate (moveDir pos dir) dir (its+1)
                | (Direction.Right, '|')
                | (Direction.Left, '|')
                        -> diagn ""
                           diagn "  vertical split"
                           iterate (moveUp pos) Direction.Up (its+1)
                           iterate (moveDown pos) Direction.Down (its+1)
                | (Direction.Up, '-')
                | (Direction.Down, '-')
                        -> diagn ""
                           diagn "  horizontal split"
                           iterate (moveLeft pos) Direction.Left (its+1)
                           iterate (moveRight pos) Direction.Right (its+1)
                | (Direction.Left, '-')
                | (Direction.Right, '-')
                        -> diagn ""
                           iterate (moveDir pos dir) dir (its+1)
                | (Direction.Up, '/')
                        -> diagn ""
                           iterate (moveRight pos) Direction.Right (its+1)
                | (Direction.Down, '/')
                        -> diagn ""
                           iterate (moveLeft pos) Direction.Left (its+1)
                | (Direction.Left, '/')
                        -> diagn ""
                           iterate (moveDown pos) Direction.Down (its+1)
                | (Direction.Right, '/')
                        -> diagn ""
                           iterate (moveUp pos) Direction.Up (its+1)
                | (Direction.Up, '\\')
                        -> diagn ""
                           iterate (moveLeft pos) Direction.Left (its+1)
                | (Direction.Down, '\\')
                        -> diagn ""
                           iterate (moveRight pos) Direction.Right (its+1)
                | (Direction.Left, '\\')
                        -> diagn ""
                           iterate (moveUp pos) Direction.Up (its+1)
                | (Direction.Right, '\\')
                        -> diagn ""
                           iterate (moveDown pos) Direction.Down (its+1)
                | (x,y) -> failwith $"Can't handle direction {x} into symbol '{y}'"
                ()

    iterate startPos startDirection 1
    ()

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 16 Part 1"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    use diag = Utility.GetTracker ()

    let input = System.IO.File.ReadAllLines(filename)

    let rc = int16 (input.Length)
    let cc = int16 (input[0].Length)
    let (dir, start, count)
        = seq {
            // start from each point down into top row, and
            // up into bottom row
            for c in 0s .. (cc-1s) do
                let p = { Row = 0s; Col = c }
                let g = buildGrid input
                applyBeams g p Direction.Down
                let eng = countEnergisedCells g
                yield (Direction.Down, p, eng)

                let p = { Row = (rc-1s); Col = c }
                let g = buildGrid input
                applyBeams g p Direction.Up
                let eng = countEnergisedCells g
                yield (Direction.Up, p, eng)

            // And now for the left and right sides
            for r in 0s .. (cc-1s) do
                let p = { Row = r; Col = 0s }
                let g = buildGrid input
                applyBeams g p Direction.Right
                let eng = countEnergisedCells g
                yield (Direction.Right, p, eng)

                let p = { Row = r; Col = (rc-1s) }
                let g = buildGrid input
                applyBeams g p Direction.Left
                let eng = countEnergisedCells g
                yield (Direction.Left, p, eng)
          }
          |> Seq.maxBy (fun (_,_,c) -> c)

    printfn ""
    printfn $"Result = {count:``#,0``} ({count})"
    printfn $"Starting {start} going {dir}"
    printfn ""
    0
