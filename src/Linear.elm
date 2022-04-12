module Linear exposing
    ( DirectionLinear(..)
    , opposite
    , ExpectedIndexInRange(..)
    )

{-| `Up` or `Down` a structure.

@docs DirectionLinear


## alter

@docs opposite


## index

@docs ExpectedIndexInRange

-}


{-| Either `Up` or `Down`.
-}
type DirectionLinear
    = Up
    | Down


{-| The other [direction](#DirectionLinear). `Down` ⇆ `Up`
-}
opposite : DirectionLinear -> DirectionLinear
opposite direction =
    case direction of
        Up ->
            Down

        Down ->
            Up


{-| Locating an index can fail for

  - negative numbers
  - numbers >= the structure's length

-}
type ExpectedIndexInRange
    = ExpectedIndexForLength Int
