module Dict.Linear exposing (foldFrom)

{-| `Dict` operations that can be applied in either [`Direction`](Linear#Direction)


## transform

@docs foldFrom

To allow any key, try [`KeySet`](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/KeySet)

-}

import Dict exposing (Dict)
import Linear exposing (Direction(..))


{-| Reduce `{ key, value }`-entries in a [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Dict

    Dict.fromList
        [ ( "Dies", 39 ), ( "Wonu", 22 ), ( "Eorfe", 18 ) ]
        |> Dict.Linear.foldFrom [] Up (\user -> (::) user.key)
    --> [ "Wonu", "Eorfe", "Dies" ]

    Dict.fromList
        [ ( "Dies", 39 ), ( "Wonu", 22 ), ( "Eorfe", 18 ) ]
        |> Dict.Linear.foldFrom [] Down (\user -> (::) user.key)
    --> [ "Dies", "Eorfe", "Wonu" ]

-}
foldFrom :
    accumulationValue
    -> Direction
    ->
        ({ key : key, value : value }
         -> (accumulationValue -> accumulationValue)
        )
    -> Dict key value
    -> accumulationValue
foldFrom accumulationValueInitial direction reduce =
    let
        fold =
            case direction of
                Up ->
                    Dict.foldl

                Down ->
                    Dict.foldr
    in
    \dict ->
        dict
            |> fold
                (\key value ->
                    { key = key, value = value } |> reduce
                )
                accumulationValueInitial
