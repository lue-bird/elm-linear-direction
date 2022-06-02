module Array.Linear exposing
    ( element
    , foldFrom
    , elementRemove, elementReplace, elementAlter, insert
    , padTo
    , take, drop, toChunks
    , squeezeIn
    )

{-| `Array` operations that can be applied in either direction.


## scan

@docs element


## transform

@docs foldFrom


### alter

@docs elementRemove, elementReplace, elementAlter, insert
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
        |> Array.Linear.insert
            ( ( Up, 1 ), \() -> 'b' )
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insert
            ( ( Down, 2 ), \() -> 'b' )
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

If the index is out of bounds, **nothing is inserted**.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insert
            ( ( Up, -1 ), \() -> 'b' )
    --> Array.fromList [ 'a', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insert
            ( ( Up, 4 ), \() -> 'b' )
    --> Array.fromList [ 'a', 'c', 'd' ]

[`squeezeIn`](#squeezeIn) allows inserting a whole `Array` of elements.

-}
insert :
    ( ( DirectionLinear, Int ), () -> element )
    -> Array element
    -> Array element
insert ( ( direction, index ), elementToInsert ) =
    squeezeIn
        ( ( direction, index )
        , \unit ->
            Array.fromList [ elementToInsert unit ]
        )


{-| Put elements of a given `Array` between the elements left and right to the given index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeIn
            ( ( Up, 1 ), \() -> Array.fromList [ 'b', 'c' ] )
    --> Array.fromList [ 'a', 'b', 'c', 'd', 'e' ]

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeIn
            ( ( Down, 2 ), \() -> Array.fromList [ 'b', 'c' ] )
    --> Array.fromList [ 'a', 'b', 'c', 'd', 'e' ]

If the index is outside of the `Array`'s range, **nothing is inserted**.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeIn
            ( ( Up, -1 ), \() -> Array.fromList [ 'b', 'c' ] )
    --> Array.fromList [ 'a', 'd', 'e' ]

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeIn
            ( ( Up, 4 ), \() -> Array.fromList [ 'b', 'c' ] )
    --> Array.fromList [ 'a', 'd', 'e' ]

[`insert`](#insert) allows inserting a single element.

-}
squeezeIn :
    ( ( DirectionLinear, Int ), () -> Array element )
    -> Array element
    -> Array element
squeezeIn ( ( direction, index ), arrayToSqueezeIn ) =
    \array ->
        if index >= 0 && index <= (array |> Array.length) then
            concat direction
                [ array |> take ( direction, index )
                , arrayToSqueezeIn ()
                , array |> drop ( direction, index )
                ]

        else
            array


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
        |> Array.Linear.element ( Down, 0 )
    --> "lose" |> Ok


    Array.fromList [ "lose", "win", "lose" ]
        |> Array.Linear.element ( Up, 0 )
    --> "lose" |> Ok

`Err` if the index is out of range.

    import Linear exposing (DirectionLinear(..), ExpectedIndexInRange(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.element ( Up, -1 )
    --> Err (ExpectedIndexForLength 3)

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.element ( Up, 100 )
    --> Err (ExpectedIndexForLength 3)

-}
element :
    ( DirectionLinear, Int )
    -> Array element
    -> Result ExpectedIndexInRange element
element ( direction, index ) =
    \array ->
        let
            length =
                array |> Array.length

            indexUp =
                case direction of
                    Up ->
                        index

                    Down ->
                        length - 1 - index
        in
        if indexUp >= 0 then
            array
                |> Array.get indexUp
                |> Result.fromMaybe
                    (ExpectedIndexForLength length)

        else
            ExpectedIndexForLength length |> Err


{-| Kick an element out of an `Array` at a given index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.Linear.elementRemove ( Up, 1 )
    --> Array.fromList [ 'a', 'b' ]

    Array.fromList [ 'a', 'b', 'c', 'd' ]
        |> Array.Linear.elementRemove ( Down, 0 )
    --> Array.fromList [ 'a', 'b', 'c' ]

If the index is out of bounds, nothing is changed.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.Linear.elementRemove ( Up, -1 )
    --> Array.fromList [ 'a', 'a', 'b' ]

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.Linear.elementRemove ( Up, 100 )
    --> Array.fromList [ 'a', 'a', 'b' ]

-}
elementRemove :
    ( DirectionLinear, Int )
    -> Array element
    -> Array element
elementRemove ( direction, index ) =
    if index >= 0 then
        \array ->
            concat direction
                [ array |> take ( direction, index )
                , array |> drop ( direction, index + 1 )
                ]

    else
        identity


{-| Set the element at an index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace
            ( ( Up, 2 ), \() -> "confusion" )
    --> Array.fromList [ "I", "am", "confusion" ]

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace
            ( ( Down, 1 ), \() -> "feel" )
    --> Array.fromList [ "I", "feel", "ok" ]

If the index is out of range, the `Array` is unaltered.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace
            ( ( Up, -1 ), \() -> "is" )
    --> Array.fromList [ "I", "am", "ok" ]

-}
elementReplace :
    ( ( DirectionLinear, Int ), () -> element )
    -> Array element
    -> Array element
elementReplace ( ( direction, index ), elementReplacement ) =
    \array ->
        let
            lastIndex =
                (array |> Array.length) - 1

            indexUp =
                case direction of
                    Up ->
                        index

                    Down ->
                        lastIndex - index
        in
        if indexUp >= 0 && indexUp <= lastIndex then
            array |> Array.set indexUp (elementReplacement ())

        else
            array


{-| Set the element at an index in a direction.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace
            ( ( Up, 2 ), String.toUpper )
    --> Array.fromList [ "I", "am", "OK" ]

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace
            ( ( Down, 0 ), String.toUpper )
    --> Array.fromList [ "I", "am", "OK" ]

If the index is out of range, the `Array` is unaltered.

    import Linear exposing (DirectionLinear(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace
            ( ( Up, -1 ), String.toUpper )
    --> Array.fromList [ "I", "am", "ok" ]

-}
elementAlter :
    ( ( DirectionLinear, Int ), element -> element )
    -> Array element
    -> Array element
elementAlter ( location, elementAlter_ ) =
    \array ->
        case array |> element location of
            Err (ExpectedIndexForLength _) ->
                array

            Ok element_ ->
                array
                    |> elementReplace
                        ( location
                        , \() -> element_ |> elementAlter_
                        )


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
                        |> insert
                            ( ( direction, 0 )
                            , \() ->
                                array |> take ( direction, chunking.length )
                            )
            }

        else
            { chunks = Array.empty, remainder = array }
