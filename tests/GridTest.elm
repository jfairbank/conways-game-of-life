module GridTest exposing (getNeighborsTest)

import Cell exposing (Cell)
import Expect
import Grid
import Test exposing (..)


sortCells : List Cell -> List Cell
sortCells =
    List.sortWith
        (\cellState1 cellState2 ->
            case ( cellState1, cellState2 ) of
                ( Cell.Alive, Cell.Dead ) ->
                    LT

                ( Cell.Dead, Cell.Alive ) ->
                    GT

                _ ->
                    EQ
        )


getNeighborsTest : Test
getNeighborsTest =
    describe "getNeighbors"
        [ test "returns 8 neighbors" <|
            \_ ->
                let
                    grid =
                        Grid.init 30 Cell.Dead
                            |> Grid.set ( 2, 3 ) Cell.Alive
                            |> Grid.set ( 1, 2 ) Cell.Alive
                            |> Grid.set ( 2, 2 ) Cell.Alive
                in
                Grid.getNeighbors ( 1, 2 ) Cell.Dead grid
                    |> sortCells
                    |> Expect.equalLists (Cell.Alive :: List.repeat 7 Cell.Dead)
        ]
