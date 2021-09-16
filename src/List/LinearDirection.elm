module List.LinearDirection exposing
    ( at
    , fold
    , order
    , group, takeFrom, dropFrom
    )

{-|


## scan

@docs at


## transform

@docs fold


## modify

@docs order


### part

@docs group, takeFrom, dropFrom

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


{-| Only use a number of elements from one side.

    [ 1, 2, 3, 4 ]
        |> List.takeFrom FirstToLast 2
    --> [ 1, 2 ]

    [ 1, 2, 3, 4 ]
        |> List.takeFrom LastToFirst 2
    --> [ 3, 4 ]

Named this way to avoid name clashes with `List.take` when importing `List.LinearDirection as List`.

-}
takeFrom : LinearDirection -> Int -> List a -> List a
takeFrom direction amount list =
    case direction of
        FirstToLast ->
            List.take amount list

        LastToFirst ->
            List.drop (List.length list - amount) list


{-| Remove a number of elements from one side.

    tail =
        List.dropFrom FirstToLast 1

    removeLast =
        List.dropFrom LastToFirst 1

Named this way to avoid name clashes with `List.drop` when importing `List.LinearDirection as List`.

-}
dropFrom : LinearDirection -> Int -> List a -> List a
dropFrom direction amount list =
    case direction of
        FirstToLast ->
            List.drop amount list

        LastToFirst ->
            List.take (List.length list - amount) list


{-| Returns `Just` the element at the given index in the list in a direction:

    [ 0, 1, 2, 3 ]
        |> List.at 0 LastToFirst
    --> Just 3

    [ 0, 1, 2, 3 ]
        |> List.at 2 FirstToLast
    --> Just 2

Returns `Nothing` if the index is out of range:

    [ 0, 1, 2, 3 ]
        |> List.at 4 FirstToLast
    --> Nothing

    [ 0, 1, 2, 3 ]
        |> List.at -1 FirstToLast
    --> Nothing

-}
at : Int -> LinearDirection -> List a -> Maybe a
at index direction =
    if index >= 0 then
        case direction of
            FirstToLast ->
                List.drop index >> List.head

            LastToFirst ->
                takeFrom LastToFirst (index + 1) >> List.head

    else
        \_ -> Nothing


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
