module List.Order exposing (greaterEarlier)

{-| `Order` `List`s

@docs greaterEarlier

-}

import Order exposing (Ordering)


{-| Order `List`s by elements first to last

    import Int.Order

    List.Order.greaterEarlier Int.Order.increasing
        [ 11, 22, 33, 188 ]
        [ 11, 22, 34 ]
    --> LT

-}
greaterEarlier : Ordering element -> Ordering (List element)
greaterEarlier elementOrder =
    Order.by listUncons
        (result
            { error = Order.tie
            , ok =
                \( head0, tail0 ) ( head1, tail1 ) ->
                    elementOrder head0 head1
                        |> onEQ (\() -> greaterEarlier elementOrder tail0 tail1)
            }
        )


{-| `Order` `Err` < `Ok`

**Should not be exposed**

-}
result :
    { error : Ordering error
    , ok : Ordering ok
    }
    -> Ordering (Result error ok)
result caseOrder =
    \result0 result1 ->
        case ( result0, result1 ) of
            ( Err error0, Err error1 ) ->
                caseOrder.error error0 error1

            ( Ok content0, Ok content1 ) ->
                caseOrder.ok content0 content1

            ( Err _, Ok _ ) ->
                LT

            ( Ok _, Err _ ) ->
                GT


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


listUncons :
    List element
    ->
        Result
            { expectedListFilled : () }
            ( element, List element )
listUncons =
    \list_ ->
        case list_ of
            [] ->
                { expectedListFilled = () } |> Err

            head :: tail ->
                ( head, tail ) |> Ok
