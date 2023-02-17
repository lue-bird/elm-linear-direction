module Array.Linear exposing
    ( element
    , foldFrom, mapFoldFrom
    , elementReplace, elementAlter
    , insert, remove
    , padToLength
    , take, drop, toChunksOf
    , attach, squeezeIn
    )

{-| `Array` operations that can be applied in either [`Direction`](Linear#Direction)


## scan

@docs element


## transform

@docs foldFrom, mapFoldFrom


### alter

@docs elementReplace, elementAlter
@docs insert, remove
@docs padToLength


### part

@docs take, drop, toChunksOf


### glueing

@docs attach, squeezeIn

-}

import Array exposing (Array)
import Linear exposing (Direction(..))
import List.Linear


{-| Reduce in a given [`Direction`](Linear#Direction)
from a given initial accumulated thing

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 'l', 'i', 'v', 'e' ]
        |> Array.Linear.foldFrom "" Up String.cons
    --> "evil"

    Array.fromList [ 'l', 'i', 'v', 'e' ]
        |> Array.Linear.foldFrom "" Down String.cons
    --> "live"

-}
foldFrom :
    accumulationValue
    -> Direction
    -> (element -> (accumulationValue -> accumulationValue))
    ->
        (Array element
         -> accumulationValue
        )
foldFrom accumulationValueInitial direction reduce =
    let
        fold =
            case direction of
                Up ->
                    Array.foldl

                Down ->
                    Array.foldr
    in
    \array -> array |> fold reduce accumulationValueInitial


{-| Map each element using information collected from previous steps,
folding in a given [`Direction`](Linear#Direction) from given initial information.

Both the mapped `Array` and the folded information will be returned

You'll often find this under the name "mapAccum"

    import Linear exposing (Direction(..))
    import Array exposing (Array)

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.mapFoldFrom 0
            Down
            (\state ->
                { element = state.folded
                , folded = state.folded + state.element
                }
            )
    --> { mapped = Array.fromList [ 5, 3, 0 ], folded = 6 }

    mapIndexed : Direction -> (Int -> a -> b) -> (Array a -> Array b)
    mapIndexed indexDirection mapAtIndex =
        Array.Linear.mapFoldFrom 0
            indexDirection
            (\state ->
                { element = state.element |> mapAtIndex state.folded
                , folded = state.folded + 1
                }
            )
            >> .mapped

    Array.fromList [ 'h', 'i', 'y', 'o' ]
        |> mapIndexed Up Tuple.pair
    --> Array.fromList [ ( 0, 'h' ), ( 1, 'i' ), ( 2, 'y' ), ( 3, 'o' ) ]

    Array.fromList [ 'h', 'i', 'y', 'o' ]
        |> mapIndexed Down Tuple.pair
    --> Array.fromList [ ( 3, 'h' ), ( 2, 'i' ), ( 1, 'y' ), ( 0, 'o' ) ]

-}
mapFoldFrom :
    accumulationValue
    -> Direction
    ->
        ({ element : element, folded : accumulationValue }
         -> { element : mappedElement, folded : accumulationValue }
        )
    ->
        (Array element
         -> { mapped : Array mappedElement, folded : accumulationValue }
        )
mapFoldFrom accumulationValueInitial direction reduce =
    \array ->
        let
            mapFolded : { mapped : List mappedElement, folded : accumulationValue }
            mapFolded =
                array
                    |> foldFrom { mapped = [], folded = accumulationValueInitial }
                        direction
                        (\element_ step ->
                            let
                                stepped : { element : mappedElement, folded : accumulationValue }
                                stepped =
                                    { element = element_, folded = step.folded } |> reduce
                            in
                            { mapped = step.mapped |> (::) stepped.element
                            , folded = stepped.folded
                            }
                        )

            mappedOrder : List a -> List a
            mappedOrder =
                case direction of
                    Up ->
                        List.reverse

                    Down ->
                        identity
        in
        { mapped = mapFolded.mapped |> mappedOrder |> Array.fromList
        , folded = mapFolded.folded
        }


{-| Put in a given element at a given index
in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insert ( Up, 1 )
            (\() -> 'b')
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insert ( Down, 2 )
            (\() -> 'b')
    --> Array.fromList [ 'a', 'b', 'c', 'd' ]

If the index is out of bounds, **nothing is inserted**

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insert ( Up, -1 )
            (\() -> 'b')
    --> Array.fromList [ 'a', 'c', 'd' ]

    Array.fromList [ 'a', 'c', 'd' ]
        |> Array.Linear.insert ( Up, 4 )
            (\() -> 'b')
    --> Array.fromList [ 'a', 'c', 'd' ]

[`squeezeIn`](#squeezeIn) allows inserting a whole `Array` of elements

-}
insert :
    ( Direction, Int )
    -> (() -> element)
    ->
        (Array element
         -> Array element
        )
insert ( direction, index ) elementToInsert =
    \array ->
        array
            |> squeezeIn ( direction, index )
                (\unit -> Array.fromList [ elementToInsert unit ])


{-| Put elements of a given `Array` between the elements left and right
to the given index in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeIn ( Up, 1 )
            (\() -> Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'b', 'c', 'd', 'e' ]

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeIn ( Down, 2 )
            (\() -> Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'b', 'c', 'd', 'e' ]

If the index is outside of the `Array`'s range, **nothing is inserted**

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeIn ( Up, -1 )
            (\() -> Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'd', 'e' ]

    Array.fromList [ 'a', 'd', 'e' ]
        |> Array.Linear.squeezeIn ( Up, 4 )
            (\() -> Array.fromList [ 'b', 'c' ])
    --> Array.fromList [ 'a', 'd', 'e' ]

[`insert`](#insert) allows inserting a single element

-}
squeezeIn :
    ( Direction, Int )
    -> (() -> Array element)
    ->
        (Array element
         -> Array element
        )
squeezeIn ( direction, index ) arrayToSqueezeIn =
    \array ->
        if index >= 0 && index <= (array |> Array.length) then
            listConcat direction
                [ array |> take direction index
                , arrayToSqueezeIn ()
                , array |> drop direction index
                ]

        else
            array


{-| Append a `List` of `Array`s in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
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
listConcat : Direction -> (List (Array element) -> Array element)
listConcat direction =
    \listOfArray ->
        listOfArray
            |> List.Linear.foldFrom
                Array.empty
                direction
                (\current soFar -> Array.append soFar current)


{-| The element at a given index in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ "lose", "win", "lose" ]
        |> Array.Linear.element ( Down, 0 )
    --> Just "lose"

    Array.fromList [ "lose", "win", "lose" ]
        |> Array.Linear.element ( Up, 0 )
    --> Just "lose"

`Nothing` if the index is out of range

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.element ( Up, -1 )
    --> Nothing

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.element ( Up, 100 )
    --> Nothing

-}
element :
    ( Direction, Int )
    ->
        (Array element
         -> Maybe element
        )
element ( direction, index ) =
    if index <= -1 then
        \_ -> Nothing

    else
        \array ->
            let
                length =
                    array |> Array.length

                indexUp =
                    case direction of
                        Up ->
                            index

                        Down ->
                            (length - 1) - index
            in
            array |> Array.get indexUp


{-| Kick an element out at a given index
in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.Linear.remove ( Up, 1 )
    --> Array.fromList [ 'a', 'b' ]

    Array.fromList [ 'a', 'b', 'c', 'd' ]
        |> Array.Linear.remove ( Down, 0 )
    --> Array.fromList [ 'a', 'b', 'c' ]

If the index is out of bounds, nothing is changed

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.Linear.remove ( Up, -1 )
    --> Array.fromList [ 'a', 'a', 'b' ]

    Array.fromList [ 'a', 'a', 'b' ]
        |> Array.Linear.remove ( Up, 100 )
    --> Array.fromList [ 'a', 'a', 'b' ]

-}
remove :
    ( Direction, Int )
    ->
        (Array element
         -> Array element
        )
remove ( direction, index ) =
    if index >= 0 then
        \array ->
            listConcat direction
                [ array |> take direction index
                , array |> drop direction (index + 1)
                ]

    else
        identity


{-| Set the element at a given index in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace ( Up, 2 )
            (\() -> "confusion")
    --> Array.fromList [ "I", "am", "confusion" ]

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace ( Down, 1 )
            (\() -> "feel")
    --> Array.fromList [ "I", "feel", "ok" ]

If the index is out of range, the `Array` is unaltered

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementReplace ( Up, -1 )
            (\() -> "is")
    --> Array.fromList [ "I", "am", "ok" ]

-}
elementReplace :
    ( Direction, Int )
    -> (() -> element)
    ->
        (Array element
         -> Array element
        )
elementReplace ( direction, index ) elementReplacement =
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


{-| Change the element at a given index in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementAlter ( Up, 2 )
            String.toUpper
    --> Array.fromList [ "I", "am", "OK" ]

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementAlter ( Down, 0 )
            String.toUpper
    --> Array.fromList [ "I", "am", "OK" ]

If the index is out of range, the `Array` is unaltered

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ "I", "am", "ok" ]
        |> Array.Linear.elementAlter ( Up, -1 )
            String.toUpper
    --> Array.fromList [ "I", "am", "ok" ]

-}
elementAlter :
    ( Direction, Int )
    -> (element -> element)
    ->
        (Array element
         -> Array element
        )
elementAlter location elementAtLocationAlter =
    \array ->
        case array |> element location of
            Nothing ->
                array

            Just elementAtLocation ->
                array
                    |> elementReplace location
                        (\() -> elementAtLocation |> elementAtLocationAlter)


{-| A given number of elements from one side
in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6 ]
        |> Array.Linear.take Up 4
        |> Array.Linear.take Down 2
    --> Array.fromList [ 3, 4 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.take Up 100
    --> Array.fromList [ 1, 2, 3 ]

The amount of elements to take is negative? → `Array.empty`

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.take Up -100
    --> Array.empty

-}
take : Direction -> Int -> (Array element -> Array element)
take direction lengthToTake =
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


{-| Remove a given number of elements from one side

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6 ]
        |> Array.Linear.drop Down 2
    --> Array.fromList [ 1, 2, 3, 4 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.drop Up 100
    --> Array.empty

Nothing is dropped if the amount of elements to drop is negative

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.drop Up -1
    --> Array.fromList [ 1, 2, 3 ]

-}
drop : Direction -> Int -> (Array element -> Array element)
drop direction lengthToDrop =
    \array ->
        array
            |> take
                (direction |> Linear.opposite)
                ((array |> Array.length) - lengthToDrop)


{-| Attach elements of a given `Array`
to the end in a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.attach Up (Array.fromList [ 4, 5, 6 ])
    --> Array.fromList [ 1, 2, 3, 4, 5, 6 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.attach Down (Array.fromList [ 4, 5, 6 ])
    --> Array.fromList [ 4, 5, 6, 1, 2, 3 ]

-}
attach : Direction -> Array element -> (Array element -> Array element)
attach direction extension =
    \array ->
        let
            ( left, right ) =
                case direction of
                    Up ->
                        ( array, extension )

                    Down ->
                        ( extension, array )
        in
        Array.append left right


{-| **Pad** in a [`Direction`](Linear#Direction) based on how much it takes to reach at a given length

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 1, 2 ]
        |> Array.Linear.padToLength Up
            (\l -> Array.repeat l 0)
            4
    --> Array.fromList [ 1, 2, 0, 0 ]

    Array.fromList [ 1, 2 ]
        |> Array.Linear.padToLength Down
            (\l -> Array.repeat l 0)
            4
    --> Array.fromList [ 0, 0, 1, 2 ]

`padToLength direction ... length |> take ( direction, length )`
→ **"resize"** behavior

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.padToLength Up
            (\l -> Array.repeat l 0)
            2
        |> Array.Linear.take Up 2
    --> Array.fromList [ 1, 2 ]

    Array.fromList [ 1, 2, 3 ]
        |> Array.Linear.padToLength Down
            (\l -> Array.repeat l 0)
            2
        |> Array.Linear.take Down 2
    --> Array.fromList [ 2, 3 ]

-}
padToLength :
    Direction
    -> (Int -> Array element)
    -> Int
    ->
        (Array element
         -> Array element
        )
padToLength directionToPadFrom paddingArrayForLength lengthMinimum =
    \array ->
        let
            paddingLength =
                lengthMinimum - (array |> Array.length)
        in
        if paddingLength >= 0 then
            array |> attach directionToPadFrom (paddingArrayForLength paddingLength)

        else
            array


{-| Split into equal-sized `chunks` of a given length in a given [`Direction`](Linear#Direction).
The left over elements to one side are in `remainder`

    import Linear exposing (Direction(..))
    import Array

    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
        |> Array.Linear.toChunksOf Up 3
    --> { chunks =
    -->     Array.fromList
    -->         [ Array.fromList [ 1, 2, 3 ]
    -->         , Array.fromList [ 4, 5, 6 ]
    -->         ]
    --> , remainder = Array.fromList [ 7 ]
    --> }

    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
        |> Array.Linear.toChunksOf Down 3
    --> { remainder = Array.fromList [ 1 ]
    --> , chunks =
    -->     Array.fromList
    -->         [ Array.fromList [ 2, 3, 4 ]
    -->         , Array.fromList [ 5, 6, 7 ]
    -->         ]
    --> }

-}
toChunksOf :
    Direction
    -> Int
    ->
        (Array element
         ->
            { chunks : Array (Array element)
            , remainder : Array element
            }
        )
toChunksOf chunkingDirection chunkLength =
    -- uses List.Linear.toChunksOf for better performance
    \array ->
        let
            chunked =
                array
                    |> Array.toList
                    |> List.Linear.toChunksOf chunkingDirection chunkLength
        in
        { remainder = chunked.remainder |> Array.fromList
        , chunks =
            chunked.chunks
                |> List.map Array.fromList
                |> Array.fromList
        }
