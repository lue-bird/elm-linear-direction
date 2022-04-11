module Set.Linear exposing (foldFrom)

{-|


## transform

@docs foldFrom

-}

import Linear exposing (DirectionLinear(..))
import Set exposing (Set)


{-| Reduce a `Set` in a direction.

    import Linear exposing (DirectionLinear(..))
    import Set

    Set.fromList [ 'i', 'l', 'a', 'g' ]
        |> Set.Linear.foldFrom ( "", Up, String.cons )
    --> "liga"

    Set.fromList [ 'i', 'l', 'a', 'g' ]
        |> Set.Linear.foldFrom ( "", Down, String.cons )
    --> "agil"

-}
foldFrom :
    ( accumulationValue
    , DirectionLinear
    , element -> accumulationValue -> accumulationValue
    )
    -> Set element
    -> accumulationValue
foldFrom ( accumulationValueInitial, direction, reduce ) =
    let
        fold =
            case direction of
                Up ->
                    Set.foldl

                Down ->
                    Set.foldr
    in
    fold reduce accumulationValueInitial
