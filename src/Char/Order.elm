module Char.Order exposing (alphabetically)

{-| `Order` `Char`s

@docs alphabetically

-}

import Case exposing (Case)
import Order exposing (Ordering)


{-| `Order` `Char`s

  - Both are letters → `Order` alphabetically
      - They're the same picture? → a given [`Ordering`](Order#Ordering) on their cases
  - Just one isn't a letter → `Order` according to unicode char code

```
import Case

Char.Order.alphabetically Case.upperLower 'b' 'D'
--> LT

Char.Order.alphabetically Case.upperLower 'l' 'L'
--> GT
```

-}
alphabetically : Ordering Case -> Ordering Char
alphabetically caseOrder =
    \char0 char1 ->
        case Maybe.map2 Tuple.pair (char0 |> charCase) (char1 |> charCase) of
            Just ( case0, case1 ) ->
                Order.by Char.toLower compare char0 char1
                    |> onEQ (\() -> caseOrder case0 case1)

            Nothing ->
                compare char0 char1


charCase : Char -> Maybe Case
charCase =
    \char_ ->
        if char_ |> Char.isLower then
            Case.Lower |> Just

        else if char_ |> Char.isUpper then
            Case.Upper |> Just

        else
            Nothing


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
