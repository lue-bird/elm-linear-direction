module LinearDirection exposing
    ( LinearDirection(..)
    , opposite
    , toFirstToLast
    )

{-| `FirstToLast`, `LastToFirst`.

@docs LinearDirection


## util

@docs opposite


### for indices or amounts

@docs toFirstToLast

-}


{-| Either `FirstToLast` or `LastToFirst`.
-}
type LinearDirection
    = FirstToLast
    | LastToFirst


{-| The other direction. `LastToFirst` ⇆ `FirstToLast`
-}
opposite : LinearDirection -> LinearDirection
opposite direction =
    case direction of
        FirstToLast ->
            LastToFirst

        LastToFirst ->
            FirstToLast


{-| Translate an amount or index to one that is used `FirstToLast`.

    import LinearDirection exposing (toFirstToLast)

    at index direction array =
        Array.get
            (index
                |> toFirstToLast direction
                    { length = Array.length array }
            )

-}
toFirstToLast : LinearDirection -> { length : Int } -> Int -> Int
toFirstToLast direction { length } n =
    case direction of
        FirstToLast ->
            n

        LastToFirst ->
            length - 1 - n
