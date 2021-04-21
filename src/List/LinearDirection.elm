module List.LinearDirection exposing (fold)

{-|


## transform

@docs fold


## part

@docs group

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


{-| Split the `Array` into equal-sized chunks. The elements left over on one side are in `less`.

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> List.group 3 FirstToLast
    --> { groups = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
    --> , less = Array.fromList [ 7 ]
    --> }

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> List.group 3 LastToFirst
    --> { less = [ 1 ]
    --> , groups = [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
    --> }

-}
group :
    Int
    -> LinearDirection
    -> Array a
    -> { groups : Array (Array a), less : Array a }
group groupSize direction array =
    if Array.length array >= groupSize then
        let
            after =
                group groupSize
                    direction
                    (drop groupSize direction array)
        in
        { groups =
            .groups after
                |> insertAt 0
                    direction
                    (take groupSize direction array)
        , less = .less after
        }

    else
        { groups = Array.empty, less = array }
