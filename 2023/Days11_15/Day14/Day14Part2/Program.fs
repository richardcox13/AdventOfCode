open System
open AoC.Common

let makeGrid (input: string array) =
    let rowCount = input.Length
    let colCount = input[0].Length
    if not (input |> Seq.forall (fun r -> r.Length = colCount)) then
        failwith "Not all input rows are the same length"
    Array2D.init rowCount colCount
        (fun r c -> input[r][c])

let printGrid title (prefix: string) (grid: char[,]) =
    Console.WriteLine(title + ":")
    for r in 0 .. ((Array2D.length1 grid) - 1) do
        Console.Write(prefix + " ")
        for c in 0 .. ((Array2D.length2 grid) - 1) do
            Console.Write(grid[r, c])
        Console.WriteLine()
    Console.WriteLine()

let tiltGridNorthOrSouth (isNorth: bool) (grid: char[,]) =
    let rowCount = Array2D.length1 grid
    let tiltOneColumn col =
        
        let nextRow curRow =
            if isNorth then curRow+1 else curRow-1

        let rec moveOneTile rowOfFirstSpace currentRow =
            if (isNorth && currentRow >= rowCount)
                    || (not isNorth && currentRow < 0) then
                ()
            else
                match (rowOfFirstSpace, grid[currentRow, col]) with
                // Found rock before finding a space... keep looking
                | (None, '#')
                | (None, 'O') -> moveOneTile None (nextRow currentRow)
                // Found a space
                | (None, '.') -> moveOneTile (Some currentRow) (nextRow currentRow)
                // Found a rock with a space before: move the rock
                | (Some spaceRow, 'O') ->
                    grid[spaceRow, col] <- 'O'
                    grid[currentRow, col] <- '.'
                    // Continue looking for another move
                    moveOneTile None (nextRow spaceRow)
                // found a cubic (unmovabnle) rock, nothing before to move,
                // but might be after, so start again
                | (Some _, '#') -> moveOneTile None (nextRow currentRow)
                // Another space: keep moving
                | (Some spaceRow, '.') -> moveOneTile (Some spaceRow) (nextRow currentRow)
                | (x, y) ->
                    let d = if isNorth then "north" else "south"
                    failwith $"Unexpected match for (%A{x}, %A{y}) while tilt us {d}"

        moveOneTile None (if isNorth then 0 else (rowCount-1))

    let colCount = Array2D.length2 grid
    for c in 0 .. (colCount - 1) do
        tiltOneColumn c

let tiltNorth = tiltGridNorthOrSouth true
let tiltSouth = tiltGridNorthOrSouth false

let calculateLoad (grid: char[,]) =
    let rowCount = Array2D.length1 grid
    let colCount =  Array2D.length2 grid
    seq {
        for r in 0 .. (rowCount-1) do
            let rowWieght = rowCount - r
            let rowCount
                = seq { 0 .. (colCount-1) }
                  |> Seq.where (fun c -> grid[r, c] = 'O')
                  |> Seq.length
            rowCount * rowWieght
    } |> Seq.sum

let hashGrid (grid: char[,]) =
    // Initial value...
    let hc = new HashCode ()
    Array2D.iter (fun c -> hc.Add(c)) grid
    hc.ToHashCode()

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 14 Part 2"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""

    (*let input = [|
        "...#.."
        "OO..#O"
        ".O.OO#"
        ".....O"
    |]*)

    let input = System.IO.File.ReadAllLines(filename)

    use diag = Utility.GetTracker ()

    let grid = makeGrid input
    printGrid "Initial grid" "  " grid

    // (load, hash-of-grid)
    let mutable history: (int * int) list = []

    for c in 1..10 do
        tiltNorth grid
        tiltSouth grid
        let load = calculateLoad grid
        let hash = hashGrid grid
        printfn $"Cycle #{c}: load = {load}, hash={hash}"
        // Reminder: will be reverse order
        history <- (load, hash) :: history

    let load = -1

    printfn ""
    printfn $"Load after tile = {load:``#,0``} ({load})"
    printfn ""
    0
