module Float.Order exposing (increasing, decreasing)

{-| `Order` `Float`s

@docs increasing, decreasing

-}

import Order exposing (Ordering)


{-| `Order` `Float`s where lower means greater

    Float.Order.increasing 40.34 2.1
    --> GT

-}
increasing : Ordering Float
increasing =
    compare


{-| `Order` `Float`s where higher means greater

    Float.Order.decreasing 2.1 40.34
    --> GT

-}
decreasing : Ordering Float
decreasing =
    increasing |> Order.reverse
