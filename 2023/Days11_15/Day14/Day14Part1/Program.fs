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

let tiltGrid (grid: char[,]) =
    let rowCount = Array2D.length1 grid
    let tiltOneColumn col =
        let rec moveOneTile rowOfFirstSpace currentRow =
            if currentRow >= rowCount then
                false
            else
                match (rowOfFirstSpace, grid[currentRow, col]) with
                // Found rock before finding a space... keep looking
                | (None, '#')
                | (None, 'O') -> moveOneTile None (currentRow+1)
                // Found a space
                | (None, '.') -> moveOneTile (Some currentRow) (currentRow+1)
                // Found a rock with a space before: move the rock
                | (Some spaceRow, 'O') ->
                    grid[spaceRow, col] <- 'O'
                    grid[currentRow, col] <- '.'
                    true
                // found a cubic (unmovabnle) rock, nothing before to move,
                // but might be after, so start again
                | (Some _, '#') -> moveOneTile None (currentRow+1)
                // Another space: keep moving
                | (Some spaceRow, '.') -> moveOneTile (Some spaceRow) (currentRow+1)
                | (x, y) -> failwith $"Unexpected match for (%A{x}, %A{y})"

        let rec applyMoveOneTile iterationCount =
            let m1 = moveOneTile None 0
            //printGrid $"After {iterationCount+1} iterations of applyMoveOneTile fpr column #{col}" "      " grid
            match m1 with
            | true -> applyMoveOneTile (iterationCount+1)
            // We're done
            | false -> ()

        applyMoveOneTile 0

    let colCount = Array2D.length2 grid
    for c in 0 .. (colCount - 1) do
        tiltOneColumn c
        //printGrid $"After column #{c}" "      " grid


[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 14 Part 1"
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

    tiltGrid grid

    printGrid "After tilt grid" "  " grid


    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
