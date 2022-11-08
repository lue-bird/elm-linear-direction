module Int.Order exposing (increasing, decreasing)

{-| `Order` `Int`s

@docs increasing, decreasing

-}

import Order exposing (Ordering)


{-| `Order` `Int`s where lower means greater

    Int.Order.increasing 40 2
    --> GT

-}
increasing : Ordering Int
increasing =
    compare


{-| `Order` `Int`s where higher means greater

    Int.Order.decreasing 2 40
    --> GT

-}
decreasing : Ordering Int
decreasing =
    increasing |> Order.reverse
