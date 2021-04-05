module LinearDirection.Array exposing
    ( fold
    , take, drop, group
    , at
    , replaceAt, removeAt, insertAt
    )

{-| `Array` operations that can be applied in either direction.


## transform

@docs fold


## part

@docs take, drop, group


## scan

@docs at


## modify

@docs replaceAt, removeAt, insertAt

-}

import Array exposing (Array)
import LinearDirection exposing (LinearDirection(..), toFirstToLast)


{-| Reduce an `Array` in a direction.

    Array.fromList [ "l", "i", "v", "e" ]
        |> Array.fold FirstToLast (++) ""
    --> "live"

    Array.fromList [ "l", "i", "v", "e" ]
        |> Array.fold LastToFirst (++) ""
    --> "evil"

-}
fold :
    LinearDirection
    -> (element -> result -> result)
    -> result
    -> Array element
    -> result
fold direction reduce initial =
    case direction of
        FirstToLast ->
            Array.foldl reduce initial

        LastToFirst ->
            Array.foldr reduce initial


{-| Put an element in an `Array` at a given index in a direction.

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.insertAt 1 FirstToLast 'b'
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]
    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.insertAt 2 LastToFirst 'b'
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

If the index is out of bounds, nothing gets inserted.

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.insertAt -1 'b'
    --> Array.fromList [ 'a', 'c', 'd' ]
    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.insertAt 10 'b'
    --> Array.fromList [ 'a', 'c', 'd' ]

-}
insertAt : Int -> LinearDirection -> a -> Array a -> Array a
insertAt index direction element array =
    if index >= 0 && index <= Array.length array then
        let
            indexAfterElement =
                case direction of
                    FirstToLast ->
                        index

                    LastToFirst ->
                        Array.length array - index

            before =
                Array.slice 0 indexAfterElement array

            after =
                Array.slice indexAfterElement (Array.length array) array
        in
        Array.append (Array.push element before) after

    else
        array


{-| Kick an element out of an `Array` at a given index in a direction.

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.removeAt 1 FirstToLast
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]
    Array.fromList [ 'a', 'b', 'c', 'd' ]
        |> Array.removeAt 0 LastToFirst
    --> Array.fromList [ 'a', 'b', 'c' ]

If the index is out of bounds, the `Array` is unaltered.

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.removeAt -1 FirstToLast
    --> Array.fromList [ 'a', 'a', 'b' ]

-}
removeAt : Int -> LinearDirection -> Array a -> Array a
removeAt index direction array =
    if index >= 0 && index <= Array.length array then
        let
            firstToLastIndex =
                toFirstToLast index
                    direction
                    { length = Array.length array }

            before =
                Array.slice 0 firstToLastIndex array

            after =
                Array.slice (firstToLastIndex + 1) (Array.length array) array
        in
        Array.append before after

    else
        array


{-| `Just` the element at an index in a direction.

    Array.fromList [ "lose", "win", "lose" ]
        |> Array.at 0 LastToFirst
    --> Just "lose"

    Array.fromList [ 1, 2, 3 ]
        |> Array.at 0 FirstToLast
    --> Just "lose"

`Nothing`, if the index is out of range.

    Array.fromList [ 1, 2, 3 ]
        |> Array.at -1
    --> Nothing

-}
at : Int -> LinearDirection -> Array a -> Maybe a
at index direction array =
    Array.get
        (toFirstToLast index
            direction
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
        (toFirstToLast index
            direction
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

    Array.fromList [ 1, 2, 3 ]
        |> Array.drop -100 FirstToLast
    --> Array.fromList [ 1, 2, 3 ]

-}
drop : Int -> LinearDirection -> Array a -> Array a
drop amount direction array =
    take (Array.length array - amount)
        (LinearDirection.opposite direction)
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
