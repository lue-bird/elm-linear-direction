## [linear direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

Q: can direction be better expressed than in

  - `foldr`, `foldl`: does `foldr` mean fold right? A quite unclear name
  - no `Array.getr/l`, `setr/l` but `foldr`, `foldl`?
  - negative indices
      - `Array.slice 0 -1` is handy! But you can't do `slice 2 -0`
      - many operations support negative indices, but others don't
      - not explicit → can have unintended side-effects

A: Use the **direction as an argument**

```elm
import LinearDirection exposing (LinearDirection(..))
import List.LinearDirection as List
import Array.LinearDirection as Array

[ 'l', 'i', 'v', 'e' ]
    |> List.fold FirstToLast String.cons ""
--> "evil"

[ 'l', 'i', 'v', 'e' ]
    |> List.fold LastToFirst String.cons ""
--> "live"

last : Array a -> Maybe a
last =
    Array.at 0 LastToFirst
```

  - → a less cluttered API (e.g. one `fold` instead of `foldr` `foldl` or `group` instead of [`chunksFromLeft`/`Right`](https://package.elm-lang.org/packages/elm-community/list-split/latest/List-Split))

  - → deal with both directions at once

    ```elm
    import LinearDirection exposing (LinearDirection)
    import Array.LinearDirection as Array

    alterAt :
        Int -> LinearDirection -> (a -> a) -> Array a -> Array a
    alterAt index direction alter =
        \array ->
            array
              |> Array.replaceAt index direction
                  (array |> Array.at index direction |> alter)
    ```

  - → direction is always explicit

## where this is already used

- [`emptiness-typed`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/)
- [`typesafe-array`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/)
- [`rosetree-path`](https://package.elm-lang.org/packages/lue-bird/elm-rosetree-path/latest/)
