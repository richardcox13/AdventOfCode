namespace AoC.Common

type grid<'t> = 't array2d

module Grid = 

    open System



    let public create<'t> rows cols (value: 't) =
        ArgumentOutOfRangeException.ThrowIfLessThanOrEqual(rows, 0)
        ArgumentOutOfRangeException.ThrowIfLessThanOrEqual(cols, 0)

        Array2D.create rows cols value

    let colCount (g: 't grid) =
        Array2D.length2 g


    let rowCount (g: 't grid) =
        Array2D.length1 g

