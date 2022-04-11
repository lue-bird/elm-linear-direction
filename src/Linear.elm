module Linear exposing
    ( DirectionLinear(..)
    , opposite
    , at, ExpectedIndexInRange(..)
    )

{-| `Up` or `Down` a structure.

@docs DirectionLinear


## alter

@docs opposite


## index

@docs at, ExpectedIndexInRange

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


{-| Save a location alongside a structure.

    import Array
    import Array.Linear

    Array.fromList [ 'a', 'c', 'd' ]
        |> Linear.at ( Down, 2 )
        |> Array.Linear.insert (\() -> 'b')
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

-}
at :
    location
    -> structure
    ->
        { structure : structure
        , location : location
        }
at location =
    \structure ->
        { structure = structure
        , location = location
        }


{-| Locating an index can fail for

  - negative numbers
  - numbers >= the structure's length

-}
type ExpectedIndexInRange
    = ExpectedIndexForLength Int
