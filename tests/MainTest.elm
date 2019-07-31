module MainTest exposing (cellTickTest)

import Cell
import Expect
import Fuzz
import Main
import Test exposing (..)


cellTickTest : Test
cellTickTest =
    describe "cellTick"
        [ test "cell dies if no live neighbors" <|
            \_ ->
                let
                    neighbors =
                        List.repeat 8 Cell.Dead
                in
                Main.cellTick Cell.Alive neighbors
                    |> Expect.equal Cell.Dead
        , test "live cell dies if 1 live neighbor" <|
            \_ ->
                let
                    neighbors =
                        Cell.Alive :: List.repeat 7 Cell.Dead
                in
                Main.cellTick Cell.Alive neighbors
                    |> Expect.equal Cell.Dead
        , test "live cell lives if live neighbors equal to 2" <|
            \_ ->
                let
                    neighbors =
                        List.repeat 2 Cell.Alive ++ List.repeat 6 Cell.Dead
                in
                Main.cellTick Cell.Alive neighbors
                    |> Expect.equal Cell.Alive
        , test "live cell lives if live neighbors equal to 3" <|
            \_ ->
                let
                    neighbors =
                        List.repeat 3 Cell.Alive ++ List.repeat 5 Cell.Dead
                in
                Main.cellTick Cell.Alive neighbors
                    |> Expect.equal Cell.Alive
        , fuzz (Fuzz.intRange 4 8) "live cell dies if live neighbors > 3" <|
            \liveNeighbors ->
                let
                    deadNeighbors =
                        8 - liveNeighbors

                    neighbors =
                        List.repeat liveNeighbors Cell.Alive ++ List.repeat deadNeighbors Cell.Dead
                in
                Main.cellTick Cell.Alive neighbors
                    |> Expect.equal Cell.Dead
        , test "dead cell dies if live neighbors equal to 2" <|
            \_ ->
                let
                    neighbors =
                        List.repeat 2 Cell.Alive ++ List.repeat 5 Cell.Dead
                in
                Main.cellTick Cell.Dead neighbors
                    |> Expect.equal Cell.Dead
        , test "dead cell lives if live neighbors equal to 3" <|
            \_ ->
                let
                    neighbors =
                        List.repeat 3 Cell.Alive ++ List.repeat 5 Cell.Dead
                in
                Main.cellTick Cell.Dead neighbors
                    |> Expect.equal Cell.Alive
        ]
