module Case exposing
    ( Case(..)
    , lowerUpper, upperLower
    )

{-| Casing

@docs Case


## order

@docs lowerUpper, upperLower

-}

import Order exposing (Ordering)


{-| `'a' < 'A'`
-}
lowerUpper : Ordering Case
lowerUpper =
    \case0 case1 ->
        case ( case0, case1 ) of
            ( Lower, Lower ) ->
                EQ

            ( Lower, Upper ) ->
                LT

            ( Upper, Lower ) ->
                GT

            ( Upper, Upper ) ->
                EQ


{-| `'A' < 'a'`
-}
upperLower : Ordering Case
upperLower =
    \case0 case1 ->
        case ( case0, case1 ) of
            ( Lower, Lower ) ->
                EQ

            ( Lower, Upper ) ->
                GT

            ( Upper, Lower ) ->
                LT

            ( Upper, Upper ) ->
                EQ


{-| `Case` of a letter. [`Ordering`](Order#Ordering)s:

  - [`Case.lowerUpper`](#lowerUpper)
  - [`Case.upperLower`](#upperLower)
  - [`Order.tie`](Order#tie)

-}
type Case
    = Lower
    | Upper
