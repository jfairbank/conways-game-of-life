module Cell exposing
    ( Cell(..)
    , isAlive
    , isDead
    )


type Cell
    = Alive
    | Dead


isAlive : Cell -> Bool
isAlive cell =
    cell == Alive


isDead : Cell -> Bool
isDead cell =
    cell == Dead
