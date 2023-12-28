module AoC.Common.Pos

open AoC.Common.Core

let moveUp pos = { Row = pos.Row - 1; Col = pos.Col }

let moveDown pos = { Row = pos.Row + 1; Col = pos.Col }

let moveRight pos = { Row = pos.Row; Col = pos.Col + 1 }

let moveLeft pos = { Row = pos.Row; Col = pos.Col - 1 }
