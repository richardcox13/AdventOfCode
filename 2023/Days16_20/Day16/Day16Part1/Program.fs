open System
open System.Collections.Generic
open System.IO
open AoC.Common


type Direction =
    | Up = 1
    | Left = 2
    | Down = 3
    | Right = 4

type Position =
    { Row: int
      Col: int }
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

let buildGrid (filename: string) =
    let input = System.IO.File.ReadAllLines(filename)
    Array2D.init input.Length input[0].Length
        (fun r c -> makeGridCell (input[r][c]))

let getCell (pos: Position) (grid: Grid) =
    grid[pos.Row, pos.Col]

let getCellSymbol pos grid = (getCell pos grid).Symbol

let getCellDirectionFlag dir cell =
    match dir with
    | Direction.Up -> cell.HasUpwardsBeam
    | Direction.Down -> cell.HasDownwardsBeam
    | Direction.Left -> cell.HasLeftwardsBeam
    | Direction.Right -> cell.HasRightwardsBeam
    | _  -> failwith $"Unexpected direction {dir}"

let rowCount grid = Array2D.length1 grid
let colCount grid = Array2D.length2 grid

let printGrid (title:string) (grid: Grid) =
    Console.Write(title)
    Console.WriteLine(':')
    let rc = rowCount grid
    let cc = colCount grid
    for r in 0 .. (rc-1) do
        Console.Write("    ")
        for c in 0 .. (cc-1) do
            Console.Write(getCellSymbol { Row = r; Col = c} grid)
        Console.WriteLine()
    Console.WriteLine()

let moveLeft pos = { Row = pos.Row; Col = pos.Col - 1 }
let moveRight pos = { Row = pos.Row; Col = pos.Col + 1 }
let moveUp pos = { Row = pos.Row - 1; Col = pos.Col }
let moveDown pos = { Row = pos.Row + 1; Col = pos.Col }

let applyBeams (grid: Grid) =
    let diag (msg: string) =
        Console.WriteLine(msg)
        ()

    let isInGrid pos =
        pos.Row >= 0
            && pos.Row < (rowCount grid)
            && pos.Col >= 0
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
    let rec iterate pos dir =
        diag $"Iterate to {pos} coming {dir}"
        if not (isInGrid pos) then
            diag "  outside grid: end of beam"
            ()
        else
            let cell = getCell pos grid
            if getCellDirectionFlag dir cell then
                diag "  this cell has already been visited in this direction: end of beam"
                ()
            else
                updateCellWithBeam pos dir
                let sym = getCellSymbol pos grid
                match (dir, sym) with
                | (x,y) -> failwith $"Can't handle direction {x} into symbol {y}"
                ()


    let startPos = { Row = 0; Col = 0 }
    let startDirection = Direction.Left
    iterate startPos startDirection
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

    let grid = buildGrid filename
    printGrid "Start" grid

    applyBeams grid

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
