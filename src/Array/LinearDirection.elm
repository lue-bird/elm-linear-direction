module Array.LinearDirection exposing
    ( at
    , replaceAt, removeAt, insertAt
    , fold, order, resize
    , take, drop, group
    , concat
    )

{-| `Array` operations that can be applied in either direction.


## scan

@docs at


## modify

@docs replaceAt, removeAt, insertAt


## transform

@docs fold, order, resize


## part

@docs take, drop, group


## unite

@docs concat

-}

import Array exposing (Array)
import LinearDirection exposing (LinearDirection(..), toFirstToLast)
import List.LinearDirection as List


{-| Reduce an `Array` in a direction.

    Array.fold FirstToLast (++) ""
        (Array.fromList [ "l", "i", "v", "e" ])
    --> "live"

    Array.fold LastToFirst (++) ""
        (Array.fromList [ "l", "i", "v", "e" ])
    --> "evil"

-}
fold :
    LinearDirection
    -> (element -> result -> result)
    -> result
    -> Array element
    -> result
fold direction reduce initial array =
    case direction of
        FirstToLast ->
            Array.foldl reduce initial array

        LastToFirst ->
            Array.foldr reduce initial array


{-| Put an element in an `Array` at a given index in a direction.

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.insertAt 1 FirstToLast 'b'
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]
    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.insertAt 2 LastToFirst 'b'
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

If the index is out of bounds, nothing gets inserted.

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.insertAt -1 FirstToLast 'b'
    --> Array.fromList [ 'a', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.insertAt 10 FirstToLast 'b'
    --> Array.fromList [ 'a', 'c', 'd' ]

-}
insertAt : Int -> LinearDirection -> a -> Array a -> Array a
insertAt index direction element array =
    if index >= 0 && index <= Array.length array then
        let
            elementCountBeforeInserted =
                index
        in
        concat direction
            [ array |> take elementCountBeforeInserted direction
            , Array.fromList [ element ]
            , array |> drop elementCountBeforeInserted direction
            ]

    else
        array


{-| Kick an element out of an `Array` at a given index in a direction.

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.removeAt 1 FirstToLast
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]
    Array.fromList [ 'a', 'b', 'c', 'd' ]
        |> Array.removeAt 0 LastToFirst
    --> Array.fromList [ 'a', 'b', 'c' ]

If the index is out of bounds, nothing is changed.

    Array.removeAt -1 FirstToLast
        (Array.fromList [ 'a', 'a', 'b' ])
    --> Array.fromList [ 'a', 'a', 'b' ]

    Array.removeAt 100 FirstToLast
        (Array.fromList [ 'a', 'a', 'b' ])
    --> Array.fromList [ 'a', 'a', 'b' ]

-}
removeAt : Int -> LinearDirection -> Array a -> Array a
removeAt index direction array =
    if index >= 0 then
        concat direction
            [ take index direction array
            , drop (index + 1) direction array
            ]

    else
        array


{-| Append multiple `Array`s in a direction.

    Array.concat FirstToLast
        [ Array.fromList [ 2, 4, 6 ]
        , Array.fromList [ 8 ]
        , Array.fromList [ 10, 12, 14 ]
        ]
    --> Array.fromList
    -->     [ 2, 4, 6, 8, 10, 12, 14 ]

    Array.concat LastToFirst
        [ Array.fromList [ 2, 4, 6 ]
        , Array.fromList [ 8 ]
        , Array.fromList [ 10, 12, 14 ]
        ]
    --> Array.fromList
    -->     [ 14, 12, 10, 8, 6, 4, 2 ]

-}
concat : LinearDirection -> List (Array a) -> Array a
concat direction arrays =
    List.fold direction
        (\current soFar -> Array.append soFar current)
        Array.empty
        arrays


{-| `Just` the element at an index in a direction.

    Array.at 0 LastToFirst
        (Array.fromList [ "lose", "win", "lose" ])
    --> Just "lose"


    Array.at 0 FirstToLast
        (Array.fromList [ "lose", "win", "lose" ])
    --> Just "lose"

`Nothing`, if the index is out of range.

    Array.at -1 FirstToLast (Array.fromList [ 1, 2, 3 ])
    --> Nothing

    Array.at 100 FirstToLast (Array.fromList [ 1, 2, 3 ])
    --> Nothing

-}
at : Int -> LinearDirection -> Array a -> Maybe a
at index direction array =
    Array.get
        (index
            |> toFirstToLast direction
                { length = Array.length array }
        )
        array


{-| Set the element at an index in a direction.

    Array.fromList [ "I", "am", "ok" ]
        |> Array.replaceAt 2 FirstToLast "confusion"
    --> Array.fromList [ "I", "am", "confusion" ]

    Array.fromList [ "I", "am", "ok" ]
        |> Array.replaceAt 1 LastToFirst "feel"
    --> Array.fromList [ "I", "feel", "ok" ]

If the index is out of range, the array is unaltered.

    Array.fromList [ "I", "am", "ok" ]
        |> Array.replaceAt -1 FirstToLast "is"
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

    Array.fromList [ 1, 2, 3, 4, 5, 6 ]
        |> Array.take 4 FirstToLast
        |> Array.take 2 LastToFirst
    --> Array.fromList [ 3, 4 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.take 100 FirstToLast
    --> Array.fromList [ 1, 2, 3 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.take -100 FirstToLast
    --> Array.empty

-}
take : Int -> LinearDirection -> Array a -> Array a
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

    Array.fromList [ 1, 2, 3, 4, 5, 6 ]
        |> Array.drop 2 LastToFirst
    --> Array.fromList [ 1, 2, 3, 4 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.drop 100 FirstToLast
    --> Array.empty

Nothing is dropped if the index is negative.

    Array.drop -1 FirstToLast
        (Array.fromList [ 1, 2, 3 ])
    --> Array.fromList [ 1, 2, 3 ]

-}
drop : Int -> LinearDirection -> Array a -> Array a
drop amount direction array =
    take (Array.length array - amount)
        (LinearDirection.opposite direction)
        array


{-| Resize an array in a direction, padding with a given value.

    Array.resize LastToFirst 4 0
        (Array.fromList [ 1, 2 ])
    --> Array.fromList [ 0, 0, 1, 2 ]

    Array.resize LastToFirst 2 0
        (Array.fromList [ 1, 2, 3 ])
    --> Array.fromList [ 2, 3 ]

    Array.resize FirstToLast 4 0
        (Array.fromList [ 1, 2 ])
    --> Array.fromList [ 1, 2, 0, 0 ]

    Array.resize FirstToLast 2 0
        (Array.fromList [ 1, 2, 3 ])
    --> Array.fromList [ 1, 2 ]

The result is an emmpty array if the index is negative.

    Array.resize LastToFirst -1 0
        (Array.fromList [ 1, 2 ])
    --> Array.empty

-}
resize : LinearDirection -> Int -> a -> Array a -> Array a
resize direction newLength defaultValue array =
    if newLength <= 0 then
        Array.empty

    else
        let
            len =
                Array.length array
        in
        case compare len newLength of
            GT ->
                take newLength direction array

            LT ->
                concat direction
                    [ array
                    , Array.repeat (newLength - len) defaultValue
                    ]

            EQ ->
                array


{-| Keep the order if `FirstToLast`, reverse if `LastToFirst`.

    Array.fromList [ 1, 2, 3 ]
        |> Array.order LastToFirst
    --> Array.fromList [ 3, 2, 1 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.order FirstToLast
    --> Array.fromList [ 1, 2, 3 ]

-}
order : LinearDirection -> Array a -> Array a
order direction array =
    case direction of
        FirstToLast ->
            array

        LastToFirst ->
            array
                |> Array.toList
                |> List.reverse
                |> Array.fromList


{-| Split the `Array` into equal-sized chunks. The elements left over on one side are in `less`.

    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
        |> Array.group 3 FirstToLast
    --> { groups =
    -->     [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
    -->         |> List.map Array.fromList
    -->         |> Array.fromList
    --> , less = Array.fromList [ 7 ]
    --> }

    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
        |> Array.group 3 LastToFirst
    --> { less = Array.fromList [ 1 ]
    --> , groups =
    -->     [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
    -->         |> List.map Array.fromList
    -->         |> Array.fromList
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
