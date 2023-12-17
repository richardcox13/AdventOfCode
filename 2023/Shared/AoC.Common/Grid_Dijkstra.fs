namespace AoC.Common

module Grid = 

    open System
    open AoC.Common.Core
    open AoC.Common.Grid

    type DijkstraResultCell<'t> = {
        Value: 't
        Distance: int
    }

    type internal DijkstraWorkingCell<'t> = {
        Value: 't
        mutable Distance: int
        mutable Unvisited: bool
    }

    let internal infinity = Int32.MaxValue

    let internal copyGridForDijkstra (g: 't grid) =
        Array2D.init (rowCount g) (colCount g)
            (fun r c -> {
                            Value = g[r,c]
                            Distance = infinity
                            Unvisited = true
                       })

    let dijkstra<'t> (getWieght: 't -> int) (start: Position) (grid: 't grid) =
        // 1. Intialised all as unvisted, and maximum distance
        let working = copyGridForDijkstra grid
        // 2. Start cell gets distance of zero
        let startCell = Grid.cell start working
        startCell.Distance <- 0
        startCell.Unvisited <- false
        ()
