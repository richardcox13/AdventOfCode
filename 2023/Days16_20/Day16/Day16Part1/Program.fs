open System
open System.Collections.Generic
open System.IO
open AoC.Common


type Directions =
    | Up = 1
    | Left = 2
    | Down = 3
    | Right = 4


type GridCaell = {
        Symbol: char
        // Already existing inbound beams
        HasUpwardsBeam: bool
        HasLeftwardsBeam: bool
        HasDownwardsBeam: bool
        HasRightwardsBeam: bool
    }

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



    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
