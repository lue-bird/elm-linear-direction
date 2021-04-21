module List.LinearDirection exposing (fold)

{-|


## transform

@docs fold

-}

import LinearDirection exposing (LinearDirection(..))


{-| Reduce a `List` in a direction.

    List.fold FirstToLast (++) ""
        [ "l", "i", "v", "e" ]
    --> "live"

    List.fold LastToFirst (++) ""
        [ "l", "i", "v", "e" ]
    --> "evil"

-}
fold :
    LinearDirection
    -> (element -> result -> result)
    -> result
    -> List element
    -> result
fold direction reduce initial =
    case direction of
        FirstToLast ->
            List.foldl reduce initial

        LastToFirst ->
            List.foldr reduce initial
