module String.Order exposing (greaterEarlier)

{-| `Order` `String`s

@docs greaterEarlier

-}

import Maybe.Order
import Order exposing (Ordering)


{-| `Order` `String`s by `Char`s first to last, specifying a [`Char` `Ordering`](Char-Order):

    import Case
    import Char.Order
    import String.Order

    String.Order.greaterEarlier (Char.Order.alphabetically Case.lowerUpper)
        "hello, human!"
        "hello, Human"
    --> LT

For string-number chunked text,
you can use [`mcordova47/`: `NaturalOrdering.compare`](https://dark.elm.dmy.fr/packages/mcordova47/elm-natural-ordering/latest/NaturalOrdering#compare)

    NaturalOrdering.compare "abc2.tff" "abc10.tff"
    --→ LT

Personally, I'd just store the `String` as something like

    type TextChunked
        = TextChunked ( String, Maybe (List ( Int, String )) )

and order that
to avoid converting too often

-}
greaterEarlier : Ordering Char -> Ordering String
greaterEarlier charOrder =
    Order.by String.uncons
        (Maybe.Order.nothingJust
            (\( char0, other0 ) ( char1, other1 ) ->
                charOrder char0 char1
                    |> onEQ (\() -> greaterEarlier charOrder other0 other1)
            )
        )


onEQ : (() -> Order) -> (Order -> Order)
onEQ orderBreakingTie =
    \order ->
        case order of
            LT ->
                LT

            EQ ->
                orderBreakingTie ()

            GT ->
                GT
