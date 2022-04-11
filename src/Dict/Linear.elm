module Dict.Linear exposing (foldFrom)

{-|


## transform

@docs foldFrom

-}

import Dict exposing (Dict)
import Linear exposing (DirectionLinear(..))


{-| Reduce `key`-`value`-entries in a `Dict` in a direction.

    import Linear exposing (DirectionLinear(..))
    import Dict

    Dict.fromList
        [ ( "Dies", 39 ), ( "Wonu", 22 ), ( "Eorfe", 18 ) ]
        |> Dict.Linear.foldFrom
            ( [], Up, \user -> (::) user.key )
    --> [ "Wonu", "Eorfe", "Dies" ]

    Dict.fromList
        [ ( "Dies", 39 ), ( "Wonu", 22 ), ( "Eorfe", 18 ) ]
        |> Dict.Linear.foldFrom
            ( [], Down, \user -> (::) user.key )
    --> [ "Dies", "Eorfe", "Wonu" ]

-}
foldFrom :
    ( accumulationValue
    , DirectionLinear
    , { key : key, value : value }
      -> accumulationValue
      -> accumulationValue
    )
    -> Dict key value
    -> accumulationValue
foldFrom ( accumulationValueInitial, direction, reduce ) =
    let
        fold =
            case direction of
                Up ->
                    Dict.foldl

                Down ->
                    Dict.foldr
    in
    fold
        (\key value ->
            { key = key, value = value } |> reduce
        )
        accumulationValueInitial
