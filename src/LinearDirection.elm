module LinearDirection exposing
    ( LinearDirection(..), opposite
    , toFirstToLast
    )

{-| `FirstToLast`, `LastToFirst`.

@docs LinearDirection, opposite


## for indices or amounts

@docs toFirstToLast

-}


{-| Either `FirstToLast` or `LastToFirst`.
You might also want to create aliases (e.g. forward and backward)
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

    at index direction =
        Array.get
            (LinearDirection.toFirstToLast index
                direction
                { length = Array.length array }
            )

-}
toFirstToLast : Int -> LinearDirection -> { length : Int } -> Int
toFirstToLast n direction { length } =
    case direction of
        FirstToLast ->
            n

        LastToFirst ->
            length - 1 - n
