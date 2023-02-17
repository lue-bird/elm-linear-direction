module Linear exposing
    ( Direction(..)
    , directionFuzz
    , directionOpposite
    )

{-| `Up` or `Down` a structure

@docs Direction


## create

@docs directionFuzz


## alter

@docs directionOpposite

-}

import Fuzz exposing (Fuzzer)


{-| Either

  - `Up` towards the end where indexes are getting bigger
  - `Down` towards the beginning where indexes are getting smaller

-}
type Direction
    = Up
    | Down


{-| The other [`Direction`](#Direction): `Down` ⇆ `Up`
-}
directionOpposite : Direction -> Direction
directionOpposite direction =
    case direction of
        Up ->
            Down

        Down ->
            Up


{-| [`Direction`](#Direction) `Fuzzer`
-}
directionFuzz : Fuzzer Direction
directionFuzz =
    Fuzz.oneOfValues [ Up, Down ]
