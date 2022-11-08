module Maybe.Order exposing (nothingJust)

{-| `Order` `Maybe`s

@docs nothingJust

-}

import Order exposing (Ordering)


{-| `Order` `Nothing` < `Just`

    import Int.Order

    Maybe.Order.nothingJust Int.Order.increasing
        (Just -99999)
        Nothing
    --> GT

-}
nothingJust : Ordering content -> Ordering (Maybe content)
nothingJust contentOrder =
    \maybe0 maybe1 ->
        case ( maybe0, maybe1 ) of
            ( Nothing, Nothing ) ->
                EQ

            ( Nothing, Just _ ) ->
                LT

            ( Just _, Nothing ) ->
                GT

            ( Just content0, Just content1 ) ->
                contentOrder content0 content1
