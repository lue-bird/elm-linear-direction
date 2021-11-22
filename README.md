# [linear-direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

I think direction can be better expressed than in

- `foldr` and `foldl`: does `foldr` mean fold right? A quite unclear name
- no `getr/l`, `setr/l`, but `foldr` and `foldl`?
- negative indices
  - `Array.slice 0 -1` is handy! But you can't do `slice 2 -0`
  - Many operations support negative indices, but others don't
  - not explicit → can have unintended side-effects

This package's simple goal is allowing you to use the direction as an argument.

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

    updateAt :
        Int -> LinearDirection -> (a -> a) -> Array a -> Array a
    updateAt index direction alter =
        Array.replaceAt index direction
            (alter (Array.at index direction))

    ```

  - → direction is always explicit
