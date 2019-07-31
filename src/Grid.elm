module Grid exposing
    ( Grid
    , all
    , get
    , getNeighbors
    , init
    , map
    , random
    , set
    , toFlattenedList
    )

import Array exposing (Array)
import Random exposing (Generator)


type alias Grid a =
    Array (Array a)


type Height
    = Height Int


type Width
    = Width Int


dimensions : Grid a -> ( Height, Width )
dimensions grid =
    let
        height =
            Array.length grid

        width =
            Array.get 0 grid
                |> Maybe.map Array.length
                |> Maybe.withDefault height
    in
    ( Height height, Width width )


get : ( Int, Int ) -> Grid a -> Maybe a
get ( i, j ) grid =
    let
        ( Height height, Width width ) =
            dimensions grid
    in
    Array.get (modBy height i - 1) grid
        |> Maybe.andThen (Array.get (modBy width j - 1))


getNeighbors : ( Int, Int ) -> a -> Grid a -> List a
getNeighbors ( i, j ) defaultValue grid =
    [ ( i - 1, j - 1 )
    , ( i - 1, j )
    , ( i - 1, j + 1 )
    , ( i, j - 1 )
    , ( i, j + 1 )
    , ( i + 1, j - 1 )
    , ( i + 1, j )
    , ( i + 1, j + 1 )
    ]
        |> List.map
            (\coords_ ->
                get coords_ grid
                    |> Maybe.withDefault defaultValue
            )


set : ( Int, Int ) -> a -> Grid a -> Grid a
set ( i, j ) element grid =
    Array.get i grid
        |> Maybe.map (\row -> Array.set i (Array.set j element row) grid)
        |> Maybe.withDefault grid


map : (( Int, Int ) -> a -> b) -> Grid a -> Grid b
map mapper =
    Array.indexedMap <|
        \i ->
            Array.indexedMap <|
                \j -> mapper ( i, j )


randomArray : Int -> Generator a -> Generator (Array a)
randomArray size generator =
    Random.list size generator
        |> Random.map Array.fromList


random : Int -> a -> List a -> Generator (Grid a)
random gridSize definiteValue otherValues =
    randomArray gridSize
        (randomArray gridSize (Random.uniform definiteValue otherValues))


init : Int -> a -> Grid a
init gridSize defaultValue =
    Array.repeat gridSize (Array.repeat gridSize defaultValue)


all : (a -> Bool) -> Grid a -> Bool
all check grid =
    grid
        |> toFlattenedList (\_ element -> Just element)
        |> List.all check


toFlattenedList : (( Int, Int ) -> a -> Maybe b) -> Grid a -> List b
toFlattenedList mapper grid =
    let
        ( Height height, Width width ) =
            dimensions grid
    in
    Array.foldr
        (\row ( ( i, j ), list ) ->
            let
                ( ( i__, _ ), list__ ) =
                    Array.foldr
                        (\element ( ( i_, j_ ), list_ ) ->
                            case mapper ( i_, j_ ) element of
                                Just item ->
                                    ( ( i_, j_ - 1 )
                                    , item :: list_
                                    )

                                Nothing ->
                                    ( ( i_, j_ - 1 )
                                    , list_
                                    )
                        )
                        ( ( i, j ), list )
                        row
            in
            ( ( i__ - 1, j ), list__ )
        )
        ( ( height - 1, width - 1 ), [] )
        grid
        |> Tuple.second
