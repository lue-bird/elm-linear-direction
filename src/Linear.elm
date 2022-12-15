module Linear exposing
    ( Direction(..)
    , directionFuzz
    , opposite
    )

{-| `Up` or `Down` a structure

@docs Direction


## create

@docs directionFuzz


## alter

@docs opposite

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
opposite : Direction -> Direction
opposite direction =
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
