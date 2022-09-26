module Linear exposing
    ( Direction(..)
    , opposite
    , directionToString
    , IndexIntOutOfRange(..)
    )

{-| `Up` or `Down` a structure

@docs Direction


## alter

@docs opposite


## transform

@docs directionToString


## index

@docs IndexIntOutOfRange

-}


{-| Either

  - `Up` towards the end where indexes are getting bigger
  - `Down` towards the beginning where indexes are getting smaller

-}
type Direction
    = Up
    | Down


{-| The other [`Direction`](#Direction): `Down` ⇆ `Up`
-}
opposite : Direction -> Direction
opposite direction =
    case direction of
        Up ->
            Down

        Down ->
            Up


{-| The [direction](#Direction)'s lowercase name
-}
directionToString : Direction -> String
directionToString direction =
    case direction of
        Up ->
            "up"

        Down ->
            "down"


{-| Locating an element at a given index `Int` fails if its

  - `<= -1`
  - `>=` the structure's length

-}
type IndexIntOutOfRange
    = IndexIntNegative
    | IndexIntBeyondElements
