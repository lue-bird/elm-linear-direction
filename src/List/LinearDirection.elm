module List.LinearDirection exposing
    ( fold, order
    , group
    )

{-|


## transform

@docs fold, order


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
fold direction reduce initial list =
    case direction of
        FirstToLast ->
            List.foldl reduce initial list

        LastToFirst ->
            List.foldr reduce initial list


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
    -> List a
    -> { groups : List (List a), less : List a }
group groupSize direction listToGroup =
    let
        groupFirstToLast list =
            if List.length list >= groupSize then
                let
                    after =
                        groupFirstToLast (List.drop groupSize list)
                in
                { groups =
                    List.take groupSize list
                        :: .groups after
                , less = .less after
                }

            else
                { groups = [], less = list }

        { groups, less } =
            groupFirstToLast (order direction listToGroup)
    in
    { groups =
        groups
            |> order direction
            |> List.map (order direction)
    , less = less |> order direction
    }


{-| Keep the order if `FirstToLast`, reverse if `LastToFirst`.

    [ 1, 2, 3 ] |> List.order LastToFirst
    --> [ 3, 2, 1 ]

    [ 1, 2, 3 ] |> List.order FirstToLast
    --> [ 1, 2, 3 ]

-}
order : LinearDirection -> List a -> List a
order direction list =
    case direction of
        FirstToLast ->
            list

        LastToFirst ->
            list |> List.reverse
