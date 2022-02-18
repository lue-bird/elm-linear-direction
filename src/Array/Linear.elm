module Array.Linear exposing
    ( at
    , replaceAt, removeAt, insertAt
    , padToLength
    , squeezeInAt
    , take, drop, toChunksOf
    , concat, foldFrom
    )

{-| `Array` operations that can be applied in either direction.


## scan

@docs at


## alter

@docs replaceAt, removeAt, insertAt
@docs padToLength


### glue

@docs squeezeInAt


### part

@docs take, drop, toChunksOf


## transform

@docs fold

-}

import Array exposing (Array)
import LinearDirection exposing (LinearDirection(..), toFirstToLast)
import List.Linear


{-| Reduce an `Array` in a direction.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 'l', 'i', 'v', 'e' ]
        |> Array.Linear.foldFrom "" FirstToLast String.cons
    --> "evil"

    Array.fromList [ 'l', 'i', 'v', 'e' ]
        |> Array.Linear.foldFrom "" LastToFirst String.cons
    --> "live"

-}
foldFrom :
    accumulationValue
    -> LinearDirection
    -> (element -> accumulationValue -> accumulationValue)
    -> Array element
    -> accumulationValue
foldFrom initialAccumulationValue direction reduce =
    let
        fold =
            case direction of
                FirstToLast ->
                    Array.foldl

                LastToFirst ->
                    Array.foldr
    in
    fold reduce initialAccumulationValue


{-| Put an element in an `Array` at a given index in a direction.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insertAt 1 FirstToLast 'b'
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insertAt 2 LastToFirst 'b'
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

If the index is out of bounds, **nothing is inserted**.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insertAt -1 FirstToLast 'b'
    --> Array.fromList [ 'a', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insertAt 4 FirstToLast 'b'
    --> Array.fromList [ 'a', 'c', 'd' ]

[`squeezeInAt`](#squeezeInAt) allows inserting a whole `Array` of elements.

-}
insertAt :
    Int
    -> LinearDirection
    -> element
    -> Array element
    -> Array element
insertAt index direction element =
    squeezeInAt index direction (Array.fromList [ element ])


{-| Put elements of a given `Array` between the elements left and right to the given index in a direction.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeInAt 1
            FirstToLast
            (Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'b', 'c', 'd', 'e' ]

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeInAt 2
            LastToFirst
            (Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'b', 'c', 'd', 'e' ]

If the index is out of bounds, **nothing is inserted**.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeInAt -1
            FirstToLast
            (Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'd', 'e' ]

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeInAt 4
            FirstToLast
            (Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'd', 'e' ]

[`insertAt`](#insertAt) allows inserting a single element.

-}
squeezeInAt :
    Int
    -> LinearDirection
    -> Array element
    -> Array element
    -> Array element
squeezeInAt index direction arrayToSqueezeIn =
    \array ->
        if index >= 0 && index <= (array |> Array.length) then
            let
                elementCountBeforeInserted =
                    index
            in
            concat direction
                [ array |> take elementCountBeforeInserted direction
                , arrayToSqueezeIn
                , array |> drop elementCountBeforeInserted direction
                ]

        else
            array


{-| Kick an element out of an `Array` at a given index in a direction.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.Linear.removeAt 1 FirstToLast
    --> Array.fromList [ 'a', 'b' ]

    Array.fromList [ 'a', 'b', 'c', 'd' ]
        |> Array.Linear.removeAt 0 LastToFirst
    --> Array.fromList [ 'a', 'b', 'c' ]

If the index is out of bounds, nothing is changed.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.Linear.removeAt -1 FirstToLast
        (Array.fromList [ 'a', 'a', 'b' ])
    --> Array.fromList [ 'a', 'a', 'b' ]

    Array.Linear.removeAt 100 FirstToLast
        (Array.fromList [ 'a', 'a', 'b' ])
    --> Array.fromList [ 'a', 'a', 'b' ]

-}
removeAt :
    Int
    -> LinearDirection
    -> Array element
    -> Array element
removeAt index direction array =
    if index >= 0 then
        concat direction
            [ take index direction array
            , drop (index + 1) direction array
            ]

    else
        array


{-| Append a `List` of `Array`s in a direction.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    [ Array.fromList [ 2, 4, 6 ]
    , Array.fromList [ 8 ]
    , Array.fromList [ 10, 12, 14 ]
    ]
        |> Array.Linear.concat FirstToLast
    --> Array.fromList
    -->     [ 2, 4, 6, 8, 10, 12, 14 ]

    [ Array.fromList [ 2, 4, 6 ]
    , Array.fromList [ 8 ]
    , Array.fromList [ 10, 12, 14 ]
    ]
        |> Array.Linear.concat LastToFirst
    --> Array.fromList
    -->     [ 10, 12, 14, 8, 2, 4, 6 ]

**Shouldn't be exposed**

-}
concat : LinearDirection -> List (Array element) -> Array element
concat direction arrays =
    List.Linear.foldFrom Array.empty
        direction
        (\current soFar -> Array.append soFar current)
        arrays


{-| `Just` the element at an index in a direction.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ "lose", "win", "lose" ]
        |> Array.Linear.at 0 LastToFirst
    --> Just "lose"


    Array.fromList [ "lose", "win", "lose" ]
        |> Array.Linear.at 0 FirstToLast
    --> Just "lose"

`Nothing`, if the index is out of range.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.at -1 FirstToLast
    --> Nothing

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.at 100 FirstToLast
    --> Nothing

-}
at : Int -> LinearDirection -> Array element -> Maybe element
at index direction array =
    Array.get
        (index
            |> toFirstToLast direction
                { length = Array.length array }
        )
        array


{-| Set the element at an index in a direction.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.replaceAt 2 FirstToLast "confusion"
    --> Array.fromList [ "I", "am", "confusion" ]

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.replaceAt 1 LastToFirst "feel"
    --> Array.fromList [ "I", "feel", "ok" ]

If the index is out of range, the array is unaltered.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.replaceAt -1 FirstToLast "is"
    --> Array.fromList [ "I", "am", "ok" ]

-}
replaceAt :
    Int
    -> LinearDirection
    -> element
    -> Array element
    -> Array element
replaceAt index direction new array =
    Array.set
        (index
            |> toFirstToLast direction
                { length = Array.length array }
        )
        new
        array


{-| A certain number of elements from one side.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6 ]
        |> Array.Linear.take 4 FirstToLast
        |> Array.Linear.take 2 LastToFirst
    --> Array.fromList [ 3, 4 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.take 100 FirstToLast
    --> Array.fromList [ 1, 2, 3 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.take -100 FirstToLast
    --> Array.empty

-}
take : Int -> LinearDirection -> Array element -> Array element
take amount direction array =
    if amount > 0 then
        case direction of
            FirstToLast ->
                Array.slice 0 amount array

            LastToFirst ->
                Array.slice -amount (Array.length array) array

    else
        Array.empty


{-| After a certain number of elements from one side.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6 ]
        |> Array.Linear.drop 2 LastToFirst
    --> Array.fromList [ 1, 2, 3, 4 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.drop 100 FirstToLast
    --> Array.empty

Nothing is dropped if the index is negative.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.drop -1 FirstToLast
    --> Array.fromList [ 1, 2, 3 ]

-}
drop : Int -> LinearDirection -> Array element -> Array element
drop amount direction =
    \array ->
        array
            |> take ((array |> Array.length) - amount)
                (direction |> LinearDirection.opposite)


{-| Resize an `Array` in a direction, always padding with the same given value.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 1, 2 ]
        |> Array.Linear.padToLength 4 LastToFirst 0
    --> Array.fromList [ 0, 0, 1, 2 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.padToLength 2 LastToFirst 0
    --> Array.fromList [ 2, 3 ]

    Array.fromList [ 1, 2 ]
        |> Array.Linear.padToLength 4 FirstToLast 0
    --> Array.fromList [ 1, 2, 0, 0 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.padToLength 2 FirstToLast 0
    --> Array.fromList [ 1, 2 ]

The result is an empty array if the index is negative.

    import LinearDirection exposing (LinearDirection(..))

    Array.fromList [ 1, 2 ]
        |> Array.Linear.padToLength -1 LastToFirst 0
    --> Array.empty

-}
padToLength :
    Int
    -> LinearDirection
    -> element
    -> Array element
    -> Array element
padToLength newLength direction defaultValue =
    if newLength <= 0 then
        \_ -> Array.empty

    else
        \array ->
            let
                length =
                    Array.length array
            in
            case compare length newLength of
                GT ->
                    take newLength direction array

                LT ->
                    concat direction
                        [ array
                        , Array.repeat (newLength - length) defaultValue
                        ]

                EQ ->
                    array


{-| Split the `Array` into equal-sized chunks. The elements left over on one side are in `less`.

    import LinearDirection exposing (LinearDirection(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
        |> Array.Linear.toChunksOf 3 FirstToLast
    --> { chunks =
    -->     Array.fromList
    -->         [ Array.fromList [ 1, 2, 3 ]
    -->         , Array.fromList [ 4, 5, 6 ]
    -->         ]
    --> , remainder = Array.fromList [ 7 ]
    --> }

    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
        |> Array.Linear.toChunksOf 3 LastToFirst
    --> { remainder = Array.fromList [ 1 ]
    --> , chunks =
    -->     Array.fromList
    -->         [ Array.fromList [ 2, 3, 4 ]
    -->         , Array.fromList [ 5, 6, 7 ]
    -->         ]
    --> }

-}
toChunksOf :
    Int
    -> LinearDirection
    -> Array element
    ->
        { chunks : Array (Array element)
        , remainder : Array element
        }
toChunksOf chunkLength direction array =
    if (array |> Array.length) >= chunkLength then
        let
            after =
                array
                    |> drop chunkLength direction
                    |> toChunksOf chunkLength direction
        in
        { after
            | chunks =
                after.chunks
                    |> insertAt 0
                        direction
                        (array |> take chunkLength direction)
        }

    else
        { chunks = Array.empty, remainder = array }
