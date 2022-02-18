## [linear direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

Q: Can direction be better expressed than in

  - `foldr`, `foldl`: does `foldr` mean fold right? A quite unclear name
  - no `Array.getr/l`, `setr/l` but `foldr`, `foldl`?
  - negative indices
      - `Array.slice 0 -1` is handy! But you can't do `slice 2 -0`
      - many operations support negative indices, but others don't
      - not explicit → can have unintended side-effects

A: Use the **direction as an argument**:

```elm
import LinearDirection exposing (LinearDirection(..))
import List.Linear
import Array exposing (Array)
import Array.Linear

[ 'l', 'i', 'v', 'e' ]
    |> List.Linear.foldFrom "" FirstToLast String.cons
--> "evil"

[ 'l', 'i', 'v', 'e' ]
    |> List.Linear.foldFrom "" LastToFirst String.cons
--> "live"

last : Array element -> Maybe element
last =
    Array.Linear.at 0 LastToFirst
```

  - → a less cluttered API, e.g.
      - `foldFrom` instead of `foldr` `foldl`
      - `toChunksOf` instead of [`chunksFromLeft`/`Right`](https://package.elm-lang.org/packages/elm-community/list-split/latest/List-Split))

  - → deal with both directions at once

    ```elm
    import LinearDirection exposing (LinearDirection)
    import Array exposing (Array)
    import Array.Linear

    alterAt :
        Int -> LinearDirection -> (a -> a) -> Array a -> Array a
    alterAt index direction alter =
        \array ->
            array
                |> Array.Linear.replaceAt index direction
                    (array
                        |> Array.Linear.at index direction
                        |> alter
                    )
    ```

  - → direction is always explicit

## where this is already being used

- [`emptiness-typed`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/)
- [`typesafe-array`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/)
- [`rosetree-path`](https://dark.elm.dmy.fr/packages/lue-bird/elm-rosetree-path/latest/)

## suggestions?

→ See [contributing.md](https://github.com/lue-bird/elm-linear-direction/blob/master/contributing.md)
