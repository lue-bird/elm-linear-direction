module List.Linear exposing
    ( at
    , take, drop
    , toChunksOf
    , foldFrom
    )

{-| `List` operations that can be applied in either direction.


## scan

@docs at


## alter


### part

@docs take, drop
@docs toChunksOf


## transform

@docs foldFrom

-}

import LinearDirection exposing (LinearDirection(..))


{-| Reduce a `List` in a direction.

    import LinearDirection exposing (LinearDirection(..))

    [ 'l', 'i', 'v', 'e' ]
        |> List.Linear.foldFrom "" FirstToLast String.cons
    --> "evil"

    [ 'l', 'i', 'v', 'e' ]
        |> List.Linear.foldFrom "" LastToFirst String.cons
    --> "live"

-}
foldFrom :
    accumulationValue
    -> LinearDirection
    -> (element -> accumulationValue -> accumulationValue)
    -> List element
    -> accumulationValue
foldFrom initialAccumulationValue direction reduce =
    let
        fold =
            case direction of
                FirstToLast ->
                    List.foldl

                LastToFirst ->
                    List.foldr
    in
    fold reduce initialAccumulationValue


{-| Split the `Array` into equal-sized chunks. The elements left over on one side are in `less`.

    import LinearDirection exposing (LinearDirection(..))

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> List.Linear.toChunksOf 3 FirstToLast
    --> { chunks = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
    --> , remainder = [ 7 ]
    --> }

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> List.Linear.toChunksOf 3 LastToFirst
    --> { remainder = [ 1 ]
    --> , chunks = [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
    --> }

-}
toChunksOf :
    Int
    -> LinearDirection
    -> List element
    ->
        { chunks : List (List element)
        , remainder : List element
        }
toChunksOf chunkLength direction listToChunk =
    let
        toChunksFirstToLast list =
            if (list |> List.length) >= chunkLength then
                let
                    after =
                        list
                            |> List.drop chunkLength
                            |> toChunksFirstToLast
                in
                { chunks =
                    (list |> List.take chunkLength)
                        :: after.chunks
                , remainder = after.remainder
                }

            else
                { chunks = [], remainder = list }

        { chunks, remainder } =
            listToChunk
                |> order direction
                |> toChunksFirstToLast
    in
    { chunks =
        chunks
            |> order direction
            |> List.map (order direction)
    , remainder = remainder |> order direction
    }


{-| Only use a number of elements from one side.

    import LinearDirection exposing (LinearDirection(..))

    [ 1, 2, 3, 4 ]
        |> List.Linear.take 2 FirstToLast
    --> [ 1, 2 ]

    [ 1, 2, 3, 4 ]
        |> List.Linear.take 2 LastToFirst
    --> [ 3, 4 ]

-}
take : Int -> LinearDirection -> List element -> List element
take amount direction =
    case direction of
        FirstToLast ->
            List.take amount

        LastToFirst ->
            \list ->
                list
                    |> List.drop ((list |> List.length) - amount)


{-| Remove a number of elements from one side.

    import LinearDirection exposing (LinearDirection(..))

    tail =
        List.Linear.drop FirstToLast 1

    removeLast =
        List.Linear.drop LastToFirst 1

-}
drop : LinearDirection -> Int -> List element -> List element
drop direction amount =
    case direction of
        FirstToLast ->
            List.drop amount

        LastToFirst ->
            \list ->
                list
                    |> List.take ((list |> List.length) - amount)


{-| Returns `Just` the element at the given index in the list in a direction:

    import LinearDirection exposing (LinearDirection(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.at 0 LastToFirst
    --> Just 3

    [ 0, 1, 2, 3 ]
        |> List.Linear.at 2 FirstToLast
    --> Just 2

Returns `Nothing` if the index is out of range:

    import LinearDirection exposing (LinearDirection(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.at 4 FirstToLast
    --> Nothing

    [ 0, 1, 2, 3 ]
        |> List.Linear.at -1 FirstToLast
    --> Nothing

If you're using this operation often, consider using an `Array` instead of a `List`
to get `O(log n)` vs. `O(n)` random access performance.

-}
at : Int -> LinearDirection -> List element -> Maybe element
at index direction =
    if index >= 0 then
        (case direction of
            FirstToLast ->
                List.drop index

            LastToFirst ->
                take (index + 1) LastToFirst
        )
            >> List.head

    else
        \_ -> Nothing


{-| Keep the order if `FirstToLast`, reverse if `LastToFirst`.

    import LinearDirection exposing (LinearDirection(..))

    [ 1, 2, 3 ] |> List.order LastToFirst
    --→ [ 3, 2, 1 ]

    [ 1, 2, 3 ] |> List.order FirstToLast
    --→ [ 1, 2, 3 ]

**Shouldn't be exposed**

-}
order : LinearDirection -> List element -> List element
order direction =
    case direction of
        FirstToLast ->
            identity

        LastToFirst ->
            List.reverse
