module Array.Linear exposing
    ( access
    , foldFrom
    , replaceWith, remove, insert
    , padTo
    , take, drop, toChunks
    , squeezeIn
    )

{-| `Array` operations that can be applied in either direction.


## scan

@docs access


## transform

@docs foldFrom


### alter

@docs replaceWith, remove, insert
@docs padTo


### part

@docs take, drop, toChunks


### glue

@docs squeezeIn

-}

import Array exposing (Array)
import Linear exposing (DirectionLinear(..), ExpectedIndexInRange(..))
import List.Linear


{-| Reduce an `Array` in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'l', 'i', 'v', 'e' ]
        |> Array.Linear.foldFrom ( "", Up, String.cons )
    --> "evil"

    Array.fromList [ 'l', 'i', 'v', 'e' ]
        |> Array.Linear.foldFrom ( "", Down, String.cons )
    --> "live"

-}
foldFrom :
    ( accumulationValue
    , DirectionLinear
    , element -> accumulationValue -> accumulationValue
    )
    -> Array element
    -> accumulationValue
foldFrom ( accumulationValueInitial, direction, reduce ) =
    let
        fold =
            case direction of
                Up ->
                    Array.foldl

                Down ->
                    Array.foldr
    in
    fold reduce accumulationValueInitial


{-| Put an element in an `Array` at a given index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'c', 'd' ]
        |> Linear.at ( Up, 1 )
        |> Array.Linear.insert (\() -> 'b')
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Linear.at ( Down, 2 )
        |> Array.Linear.insert (\() -> 'b')
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

If the index is out of bounds, **nothing is inserted**.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'c', 'd' ]
        |> Linear.at ( Up, -1 )
        |> Array.Linear.insert (\() -> 'b')
    --> Array.fromList [ 'a', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Linear.at ( Up, 4 )
        |> Array.Linear.insert (\() -> 'b')
    --> Array.fromList [ 'a', 'c', 'd' ]

[`squeezeIn`](#squeezeIn) allows inserting a whole `Array` of elements.

-}
insert :
    (() -> element)
    ->
        { structure : Array element
        , location : ( DirectionLinear, Int )
        }
    -> Array element
insert elementToInsert =
    squeezeIn
        (\unit ->
            Array.fromList [ elementToInsert unit ]
        )


{-| Put elements of a given `Array` between the elements left and right to the given index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'd', 'e' ]
        |> Linear.at ( Up, 1 )
        |> Array.Linear.squeezeIn
            (\() -> Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'b', 'c', 'd', 'e' ]

    Array.fromList [ 'a', 'd', 'e' ]
        |> Linear.at ( Down, 2 )
        |> Array.Linear.squeezeIn
            (\() -> Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'b', 'c', 'd', 'e' ]

If the index is out of bounds, **nothing is inserted**.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'd', 'e' ]
        |> Linear.at ( Up, -1 )
        |> Array.Linear.squeezeIn
            (\() -> Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'd', 'e' ]

    Array.fromList [ 'a', 'd', 'e' ]
        |> Linear.at ( Up, 4 )
        |> Array.Linear.squeezeIn
            (\() -> Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'd', 'e' ]

[`insert`](#insert) allows inserting a single element.

-}
squeezeIn :
    (() -> Array element)
    ->
        { structure : Array element
        , location : ( DirectionLinear, Int )
        }
    -> Array element
squeezeIn arrayToSqueezeIn =
    \array ->
        let
            ( direction, index ) =
                array.location
        in
        if index >= 0 && index <= (array.structure |> Array.length) then
            concat direction
                [ array.structure |> take ( direction, index )
                , arrayToSqueezeIn ()
                , array.structure |> drop ( direction, index )
                ]

        else
            array.structure


{-| Kick an element out of an `Array` at a given index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'a', 'b' ]
        |> Linear.at ( Up, 1 )
        |> Array.Linear.remove
    --> Array.fromList [ 'a', 'b' ]

    Array.fromList [ 'a', 'b', 'c', 'd' ]
        |> Linear.at ( Down, 0 )
        |> Array.Linear.remove
    --> Array.fromList [ 'a', 'b', 'c' ]

If the index is out of bounds, nothing is changed.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'a', 'b' ]
        |> Linear.at ( Up, -1 )
        |> Array.Linear.remove
    --> Array.fromList [ 'a', 'a', 'b' ]

    Array.fromList [ 'a', 'a', 'b' ]
        |> Linear.at ( Up, 100 )
        |> Array.Linear.remove
    --> Array.fromList [ 'a', 'a', 'b' ]

-}
remove :
    { structure : Array element
    , location : ( DirectionLinear, Int )
    }
    -> Array element
remove =
    \array ->
        let
            ( direction, index ) =
                array.location
        in
        if index >= 0 then
            concat direction
                [ array.structure |> take ( direction, index )
                , array.structure |> drop ( direction, index + 1 )
                ]

        else
            array.structure


{-| Append a `List` of `Array`s in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.Linear.concat Up
        [ Array.fromList [ 2, 4, 6 ]
        , Array.fromList [ 8 ]
        , Array.fromList [ 10, 12, 14 ]
        ]
    --→ Array.fromList
    --→     [ 2, 4, 6, 8, 10, 12, 14 ]

    Array.Linear.concat Down
        [ Array.fromList [ 2, 4, 6 ]
        , Array.fromList [ 8 ]
        , Array.fromList [ 10, 12, 14 ]
        ]
    --→ Array.fromList
    --→     [ 10, 12, 14, 8, 2, 4, 6 ]

**Shouldn't be exposed**

-}
concat : DirectionLinear -> List (Array element) -> Array element
concat direction arrays =
    arrays
        |> List.Linear.foldFrom
            ( Array.empty
            , direction
            , \current soFar -> Array.append soFar current
            )


{-| The element at an index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ "lose", "win", "lose" ]
        |> Linear.at ( Down, 0 )
        |> Array.Linear.access
    --> "lose" |> Ok


    Array.fromList [ "lose", "win", "lose" ]
        |> Linear.at ( Up, 0 )
        |> Array.Linear.access
    --> "lose" |> Ok

`Err` if the index is out of range.

    import Linear exposing (DirectionLinear(..), ExpectedIndexInRange(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Linear.at ( Up, -1 )
        |> Array.Linear.access
    --> Err (ExpectedIndexForLength 3)

    Array.fromList [ 1, 2, 3 ]
        |> Linear.at ( Up, 100 )
        |> Array.Linear.access
    --> Err (ExpectedIndexForLength 3)

-}
access :
    { structure : Array element
    , location : ( DirectionLinear, Int )
    }
    -> Result ExpectedIndexInRange element
access =
    \array ->
        let
            length =
                -- O(1)
                array.structure |> Array.length

            indexUp =
                let
                    ( direction, index ) =
                        array.location
                in
                case direction of
                    Up ->
                        index

                    Down ->
                        length - 1 - index
        in
        if indexUp >= 0 then
            array.structure
                |> Array.get indexUp
                |> Result.fromMaybe
                    (ExpectedIndexForLength length)

        else
            ExpectedIndexForLength length |> Err


{-| Set the element at an index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Linear.at ( Up, 2 )
        |> Array.Linear.replaceWith (\() -> "confusion")
    --> Array.fromList [ "I", "am", "confusion" ]

    Array.fromList [ "I", "am", "ok" ]
        |> Linear.at ( Down, 1 )
        |> Array.Linear.replaceWith (\() -> "feel")
    --> Array.fromList [ "I", "feel", "ok" ]

If the index is out of range, the array is unaltered.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Linear.at ( Up, -1 )
        |> Array.Linear.replaceWith (\() -> "is")
    --> Array.fromList [ "I", "am", "ok" ]

-}
replaceWith :
    (() -> element)
    ->
        { structure : Array element
        , location : ( DirectionLinear, Int )
        }
    -> Array element
replaceWith elementReplacement =
    \array ->
        array.structure
            |> Array.set
                (let
                    ( direction, index ) =
                        array.location
                 in
                 case direction of
                    Up ->
                        index

                    Down ->
                        (array.structure |> Array.length) - 1 - index
                )
                (elementReplacement ())


{-| A given number of elements from one side.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6 ]
        |> Array.Linear.take ( Up, 4 )
        |> Array.Linear.take ( Down, 2 )
    --> Array.fromList [ 3, 4 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.take ( Up, 100 )
    --> Array.fromList [ 1, 2, 3 ]

`Array.empty` if the amount of elements to take is negative.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.take ( Up, -100 )
    --> Array.empty

-}
take : ( DirectionLinear, Int ) -> Array element -> Array element
take ( direction, lengthToTake ) =
    if lengthToTake > 0 then
        case direction of
            Up ->
                Array.slice 0 lengthToTake

            Down ->
                \array ->
                    array
                        |> Array.slice
                            -lengthToTake
                            (array |> Array.length)

    else
        \_ -> Array.empty


{-| Remove a given number of elements from one side.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6 ]
        |> Array.Linear.drop ( Down, 2 )
    --> Array.fromList [ 1, 2, 3, 4 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.drop ( Up, 100 )
    --> Array.empty

Nothing is dropped if the amount of elements to drop is negative.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.drop ( Up, -1 )
    --> Array.fromList [ 1, 2, 3 ]

-}
drop : ( DirectionLinear, Int ) -> Array element -> Array element
drop ( direction, lengthToDrop ) =
    \array ->
        array
            |> take
                ( direction |> Linear.opposite
                , (array |> Array.length) - lengthToDrop
                )


{-| **Pad** an `Array` in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 1, 2 ]
        |> Array.Linear.padTo
            { lengthMinimum = 4
            , pad = ( Up, \n -> Array.repeat n 0 )
            }
    --> Array.fromList [ 1, 2, 0, 0 ]

    Array.fromList [ 1, 2 ]
        |> Array.Linear.padTo
            { lengthMinimum = 4
            , pad = ( Down, \n -> Array.repeat n 0 )
            }
    --> Array.fromList [ 0, 0, 1, 2 ]

`padTo { .. length .. direction .. } |> take ( direction, length )`
→ **"resize"** behavior

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.padTo
            { lengthMinimum = 2
            , pad = ( Up, \n -> Array.repeat n 0 )
            }
        |> Array.Linear.take ( Up, 2 )
    --> Array.fromList [ 1, 2 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.padTo
            { lengthMinimum = 2
            , pad = ( Down, \n -> Array.repeat n 0 )
            }
        |> Array.Linear.take ( Down, 2 )
    --> Array.fromList [ 2, 3 ]

-}
padTo :
    { lengthMinimum : Int
    , pad : ( DirectionLinear, Int -> Array element )
    }
    -> Array element
    -> Array element
padTo padding =
    \array ->
        let
            paddingLength =
                padding.lengthMinimum - (array |> Array.length)

            ( paddingSide, pad ) =
                padding.pad
        in
        if paddingLength >= 0 then
            case paddingSide of
                Up ->
                    Array.append array (pad paddingLength)

                Down ->
                    Array.append (pad paddingLength) array

        else
            array


{-| Split the `Array` into equal-`length` `chunks`.
The left over elements to one side are in `remainder`.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
        |> Array.Linear.toChunks
            { length = 3, remainder = Up }
    --> { chunks =
    -->     Array.fromList
    -->         [ Array.fromList [ 1, 2, 3 ]
    -->         , Array.fromList [ 4, 5, 6 ]
    -->         ]
    --> , remainder = Array.fromList [ 7 ]
    --> }

    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
        |> Array.Linear.toChunks
            { length = 3, remainder = Down }
    --> { remainder = Array.fromList [ 1 ]
    --> , chunks =
    -->     Array.fromList
    -->         [ Array.fromList [ 2, 3, 4 ]
    -->         , Array.fromList [ 5, 6, 7 ]
    -->         ]
    --> }

-}
toChunks :
    { length : Int
    , remainder : DirectionLinear
    }
    -> Array element
    ->
        { chunks : Array (Array element)
        , remainder : Array element
        }
toChunks chunking =
    \array ->
        if (array |> Array.length) >= chunking.length then
            let
                direction =
                    chunking.remainder

                after =
                    array
                        |> drop ( direction, chunking.length )
                        |> toChunks chunking
            in
            { after
                | chunks =
                    after.chunks
                        |> Linear.at ( direction, 0 )
                        |> insert
                            (\() ->
                                array |> take ( direction, chunking.length )
                            )
            }

        else
            { chunks = Array.empty, remainder = array }
