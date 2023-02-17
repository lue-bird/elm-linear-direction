## [linear direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

  - `-r`/`-l`
      - for example: `foldr`, `foldl`
      - does `foldr` mean fold to the right? unclear and easy to mix up
      - no `Array.getr/l`, `setr/l` but `foldr`, `foldl`?
  - negative indices
      - `Array.slice 0 -1` is handy! But you can't do `slice 2 -0`
      - many operations support negative indices, but others don't
      - not explicit → can have unintended side-effects

How about taking the **[`Direction`](Linear#Direction) as an argument**!

```elm
import Linear exposing (Direction(..))
import List.Linear
import Array exposing (Array)
import Array.Linear

[ 'l', 'i', 'v', 'e' ]
    |> List.Linear.foldFrom "" Up String.cons
--> "evil"

[ 'l', 'i', 'v', 'e' ]
    |> List.Linear.foldFrom "" Down String.cons
--> "live"

Array.fromList [ 'e', 'v', 'i', 'l' ]
    |> Array.Linear.element ( Down, 0 )
--> Just 'l'
```

  - → a less cluttered API, e.g.
      - `foldFrom` instead of `foldr`, `foldl`
      - `toChunksOf` instead of [`chunksFromLeft`/-`Right`](https://package.elm-lang.org/packages/elm-community/list-split/latest/List-Split)
      - `padToAtLeast` instead of `resizerRepeat`, `resizelRepeat`, `resizerIndexed`, `resizelIndexed`

  - → deal with both directions at once

    ```elm
    import Linear exposing (DirectionLinear)
    import Array exposing (Array)
    import Array.Linear

    elementAlter :
        ( DirectionLinear, Int )
        -> (element -> element)
        -> (Array element -> Array element)
    elementAlter location alter =
        \array ->
            case array |> Array.Linear.element location of
                Nothing ->
                    array
                    
                Just elementAtLocation ->
                    array
                        |> Array.Linear.elementReplace location
                            (\() -> elementAtLocation |> alter)
    ```

  - → direction is always explicit

## where `linear direction` is already being used

  - [`emptiness-typed`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/)
  - [`typesafe-array`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/)
  - [`keysset`](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/)
  - [`rosetree-path`](https://dark.elm.dmy.fr/packages/lue-bird/elm-rosetree-path/latest/)

## suggestions?

→ [contribute](https://github.com/lue-bird/elm-linear-direction/blob/master/contributing.md)
