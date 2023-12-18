namespace AoC.Common

type grid<'t> = 't array2d

module Grid = 

    open System
    open AoC.Common.Core

    let public create<'t> rows cols (value: 't) =
        ArgumentOutOfRangeException.ThrowIfLessThanOrEqual(rows, 0)
        ArgumentOutOfRangeException.ThrowIfLessThanOrEqual(cols, 0)

        Array2D.create rows cols value

    let cell (position) (g: 't grid) =
        g[position.Row, position.Col]

    let cellRC row col (g: 't grid) =
        g[row, col]

    let colCount (g: 't grid) =
        Array2D.length2 g

    let maxCol (g: 't grid) =
        (Array2D.length2 g) - 1

    let maxRow (g: 't grid) =
        (Array2D.length1 g) - 1

    let printf (title: string) (indent: string) (formatter: 't -> string) (g: 't grid)=
        Console.Write(title)
        Console.WriteLine(":")
        for r in 0 .. (maxRow g) do
            Console.Write(indent)
            for c in 0 .. (maxCol g) do
                Console.Write(formatter (g[r,c]))
            Console.WriteLine()

    let rowCount (g: 't grid) =
        Array2D.length1 g

    let sumBy<'T> (project: 'T -> int) (g: 'T grid) =
        seq {
            let mr = maxRow g
            let mc = maxCol g
            for r in 0 .. mr do
                for c in 0 .. mc do
                    yield g[r,c] |> project
        }
        |> Seq.sum
