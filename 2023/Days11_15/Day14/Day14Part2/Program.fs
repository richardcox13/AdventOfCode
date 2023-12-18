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

let tiltGridEastOrWest (isEast: bool)  (grid: char[,]) =
    let colCount = Array2D.length2 grid
    let tiltOneRow row =
        let nextCol curCol =
            if isEast then curCol-1 else curCol+1

        let rec moveTilesInRow colOfLastSpace currentCol =
            if (isEast && currentCol < 0)
                    || (not isEast && currentCol >= colCount) then
                ()
            else
                match (colOfLastSpace, grid[row, currentCol]) with
                | (None, '#')
                | (None, 'O') -> moveTilesInRow None (nextCol currentCol)
                | (None, '.') -> moveTilesInRow (Some currentCol) (nextCol currentCol)
                | (Some spaceCol, '#')
                              -> moveTilesInRow None (nextCol currentCol)
                | (Some spaceCol, 'O')
                              -> grid[row, spaceCol] <- 'O'
                                 grid[row, currentCol] <- '.'
                                 moveTilesInRow None (nextCol spaceCol)
                | (Some spaceCol, '.')
                             -> moveTilesInRow (Some spaceCol) (nextCol currentCol)

                | (x, y) ->
                    let d = if isEast then "east" else "west"
                    failwith $"Unexpected match for (%A{x}, %A{y}) while tilt is {d}"
        moveTilesInRow None (if isEast then (colCount-1) else 0)

    let rowCount = Array2D.length1 grid
    for r in 0 .. (rowCount-1) do
        tiltOneRow r

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

let tiltEast = tiltGridEastOrWest true
let tiltNorth = tiltGridNorthOrSouth true
let tiltSouth = tiltGridNorthOrSouth false
let tiltWest = tiltGridEastOrWest false

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
    //let hc = new HashCode ()
    //Array2D.iteri (fun row col c -> hc.Add(row * col * (int c))) grid
    //hc.ToHashCode()
    let maxRow = (Array2D.length1 grid) - 1
    let maxCol = (Array2D.length2 grid) - 1
    let rec inner row col (hasher: HashCode)=
        if row > maxRow then
            hasher.ToHashCode()
        else if col > maxCol then
            inner (row+1) 0 hasher
        else
            hasher.Add(grid[row, col])
            inner row (col+1) hasher
    inner 0 0 (new HashCode())

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

    // (cycle, load, hash-of-grid)
    let mutable history: (int * int * int) list = []
    let mutable foundCycle = false
    for c in (seq {1..1_000_000} |> Seq.where (fun _ -> not foundCycle)) do
        tiltNorth grid
        //printGrid $"Ater cycle #{c} North" "    " grid
        tiltWest grid
        //printGrid $"Ater cycle #{c} West" "    " grid
        tiltSouth grid
        //printGrid $"Ater cycle #{c} South" "    " grid
        tiltEast grid
        //printGrid $"Ater cycle #{c}" "  " grid
        let load = calculateLoad grid
        let hash = hashGrid grid
        printfn $"Cycle #{c}: load = {load}, hash={hash}"

        if not foundCycle then
            match (history |> List.tryFind (fun (c, ll, hh) -> ll = load && hh = hash)) with
            | None -> ()
            | Some (prevCycle, _, _)
                   -> printfn $"  load & hash match cycle #{prevCycle}"
                      let cycleStart = prevCycle
                      let cycleLen = c - prevCycle
                      printfn $"Cycle start after cycle {cycleStart}, with length {cycleLen}"
                      let resCycle = (1_000_000_000 - cycleStart) % cycleLen + cycleStart
                      printfn $"Cucle 1000000000 = {resCycle}"
                      let (_, result, _) = history |> List.find (fun (cc, _, _) -> cc = resCycle)
                      printfn $"Result load = {result:``#,0``} ({result})"
                      foundCycle <- true
                      ()

        // Reminder: will be reverse order
        history <- (c, load, hash) :: history

    0
