open System
open System.Collections.Generic
open System.IO
open AoC.Common


type Directions =
    | Up = 1
    | Left = 2
    | Down = 3
    | Right = 4

type Position = {
        Row: int
        Col: int
    }

type GridCell = {
        Symbol: char
        // Already existing inbound beams
        HasUpwardsBeam: bool
        HasLeftwardsBeam: bool
        HasDownwardsBeam: bool
        HasRightwardsBeam: bool
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


    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
