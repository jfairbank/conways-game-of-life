module Main exposing (Grid, cellTick, init, main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyUp)
import Canvas exposing (rect, shapes, text)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Cell exposing (Cell)
import Color
import Grid
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Json
import List.Extra
import Random



-- MISC TYPES --


type TopMessage
    = TopMessage String


type BottomMessage
    = BottomMessage String


type alias Messages =
    ( TopMessage, BottomMessage )


type FrameRate
    = FrameRate Float


type ElapsedTime
    = ElapsedTime Float


type PreviousElapsedTime
    = PreviousElapsedTime Float


type AdvanceState
    = AdvanceGame
    | WaitToAdvance PreviousElapsedTime


type alias Grid =
    Grid.Grid Cell


type GameContinuation
    = Continue Grid
    | GameOver



-- CONFIG --


frameRate : FrameRate
frameRate =
    FrameRate 4


cellSize : Int
cellSize =
    20


gridSize : Int
gridSize =
    30


gridWidth : Int
gridWidth =
    gridSize * cellSize


gridHeight : Int
gridHeight =
    gridSize * cellSize


initialGameMessages : Messages
initialGameMessages =
    ( TopMessage "Conway's Game of Life"
    , BottomMessage "Press Any Key to Start"
    )


startOverMessages : Messages
startOverMessages =
    ( TopMessage "Game Over"
    , BottomMessage "Press Any Key to Start Over"
    )



-- MODEL --


type Model
    = Ready Messages
    | Running Grid PreviousElapsedTime


ready : Model
ready =
    Ready initialGameMessages


running : Grid -> Model
running grid =
    Running grid <| PreviousElapsedTime 0


done : Model
done =
    Ready startOverMessages



-- INITIALIZATION --


generateRandomGrid : Cmd Msg
generateRandomGrid =
    Grid.random gridSize Cell.Alive [ Cell.Dead ]
        |> Random.generate ReceiveGrid


init : () -> ( Model, Cmd Msg )
init () =
    ( ready, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div
        [ style "border" "5px solid #ccc"
        , style "height" <| String.fromInt gridHeight ++ "px"
        , style "margin" "100px auto 0 auto"
        , style "padding" "20px"
        , style "width" <| String.fromInt gridWidth ++ "px"
        ]
        [ Canvas.toHtml ( gridWidth, gridHeight ) [] <|
            render model
        ]


render : Model -> List Canvas.Renderable
render model =
    clearScreen :: renderContent model


renderContent : Model -> List Canvas.Renderable
renderContent =
    let
        x : Float
        x =
            toFloat gridWidth / 2

        y : Float
        y =
            toFloat gridHeight / 4

        text_ : Canvas.Point -> String -> Canvas.Renderable
        text_ =
            text
                [ align Center
                , font { size = 40, family = "Helvetic, Arial, sans-serif" }
                ]

        topText : TopMessage -> Canvas.Renderable
        topText (TopMessage message) =
            text_ ( x, y ) message

        bottomText : BottomMessage -> Canvas.Renderable
        bottomText (BottomMessage message) =
            text_ ( x, y + 100 ) message
    in
    \model ->
        case model of
            Ready ( topMessage, bottomMessage ) ->
                [ topText topMessage
                , bottomText bottomMessage
                ]

            Running grid _ ->
                Grid.toFlattenedList renderCell grid


renderCell : ( Int, Int ) -> Cell -> Maybe Canvas.Renderable
renderCell =
    let
        floatCellSize : Float
        floatCellSize =
            toFloat cellSize
    in
    \( i, j ) cell ->
        case cell of
            Cell.Alive ->
                Just <|
                    shapes []
                        [ rect ( toFloat i * floatCellSize, toFloat j * floatCellSize )
                            floatCellSize
                            floatCellSize
                        ]

            Cell.Dead ->
                Nothing


clearScreen : Canvas.Renderable
clearScreen =
    shapes [ fill Color.white ]
        [ rect ( 0, 0 ) (toFloat gridWidth) (toFloat gridHeight) ]



-- UPDATE --


type Msg
    = Start
    | ReceiveGrid Grid
    | Tick ElapsedTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start, Ready _ ) ->
            ( model, generateRandomGrid )

        ( ReceiveGrid grid, Ready _ ) ->
            ( running grid, Cmd.none )

        ( Tick elapsed, Running grid previousElapsed ) ->
            case advance elapsed previousElapsed frameRate of
                AdvanceGame ->
                    case tick grid of
                        Continue newGrid ->
                            ( running newGrid, Cmd.none )

                        GameOver ->
                            ( done, Cmd.none )

                WaitToAdvance newElapsed ->
                    ( Running grid newElapsed, Cmd.none )

        _ ->
            ( model, Cmd.none )


advance : ElapsedTime -> PreviousElapsedTime -> FrameRate -> AdvanceState
advance (ElapsedTime previousElapsed) (PreviousElapsedTime elapsed) (FrameRate frameRate_) =
    let
        newElapsed : Float
        newElapsed =
            elapsed + previousElapsed
    in
    if newElapsed > (1000 / frameRate_) then
        AdvanceGame

    else
        WaitToAdvance <| PreviousElapsedTime newElapsed


tick : Grid -> GameContinuation
tick grid =
    let
        newGrid : Grid
        newGrid =
            Grid.map
                (\( i, j ) cell ->
                    Grid.getNeighbors ( i, j ) Cell.Dead grid
                        |> cellTick cell
                )
                grid
    in
    if Grid.all Cell.isDead newGrid then
        GameOver

    else
        Continue newGrid


cellTick : Cell -> List Cell -> Cell
cellTick cell neighbors =
    let
        liveNeighbors : Int
        liveNeighbors =
            List.Extra.count Cell.isAlive neighbors
    in
    case ( liveNeighbors, cell ) of
        ( 2, Cell.Alive ) ->
            Cell.Alive

        ( 3, _ ) ->
            Cell.Alive

        _ ->
            Cell.Dead



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Ready _ ->
            onKeyUp
                (Json.field "key" Json.string
                    |> Json.map (always Start)
                )

        Running _ _ ->
            onAnimationFrameDelta (Tick << ElapsedTime)



-- PROGRAM --


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
