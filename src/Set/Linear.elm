module Set.Linear exposing (foldFrom)

{-| `Set` operations that can be applied in either [`Direction`](Linear#Direction)


## transform

@docs foldFrom

To allow any key, try [`KeySet`](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/KeySet)

-}

import Linear exposing (Direction(..))
import Set exposing (Set)


{-| Reduce in a [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Set

    Set.fromList [ 'i', 'l', 'a', 'g' ]
        |> Set.Linear.foldFrom "" Up String.cons
    --> "liga"

    Set.fromList [ 'i', 'l', 'a', 'g' ]
        |> Set.Linear.foldFrom "" Down String.cons
    --> "agil"

-}
foldFrom :
    accumulationValue
    -> Direction
    -> (element -> (accumulationValue -> accumulationValue))
    ->
        (Set element
         -> accumulationValue
        )
foldFrom accumulationValueInitial direction reduce =
    let
        fold =
            case direction of
                Up ->
                    Set.foldl

                Down ->
                    Set.foldr
    in
    \set -> set |> fold reduce accumulationValueInitial
