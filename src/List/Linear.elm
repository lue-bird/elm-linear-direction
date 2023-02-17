module List.Linear exposing
    ( element
    , elementAlter
    , foldFrom, mapFoldFrom
    , take, drop
    , toChunksOf
    )

{-| `List` operations that can be applied in either [`Direction`](Linear#Direction)


## scan

@docs element


## alter

@docs elementAlter


## transform

@docs foldFrom, mapFoldFrom


### part

@docs take, drop
@docs toChunksOf

-}

import Linear exposing (Direction(..))


{-| Reduce in a [`Direction`](Linear#Direction)
from a given initial accumulated thing

    import Linear exposing (Direction(..))

    [ 'l', 'i', 'v', 'e' ]
        |> List.Linear.foldFrom "" Up String.cons
    --> "evil"

    [ 'l', 'i', 'v', 'e' ]
        |> List.Linear.foldFrom "" Down String.cons
    --> "live"

-}
foldFrom :
    accumulationValue
    -> Direction
    -> (element -> (accumulationValue -> accumulationValue))
    ->
        (List element
         -> accumulationValue
        )
foldFrom accumulationValueInitial direction reduce =
    let
        fold =
            case direction of
                Up ->
                    List.foldl

                Down ->
                    List.foldr
    in
    \list -> list |> fold reduce accumulationValueInitial


{-| Map each element using information collected from previous steps,
folding in a given [`Direction`](Linear#Direction) from given initial information.

Both the mapped `Array` and the folded information will be returned

You'll often find this under the name "mapAccum"

    import Linear exposing (Direction(..))

    [ 1, 2, 3 ]
        |> List.Linear.mapFoldFrom 0
            Down
            (\state ->
                { element = state.folded
                , folded = state.folded + state.element
                }
            )
    --> { mapped = [ 5, 3, 0 ], folded = 6 }

    mapIndexed : Direction -> (Int -> a -> b) -> (List a -> List b)
    mapIndexed indexDirection mapAtIndex =
        List.Linear.mapFoldFrom 0
            indexDirection
            (\state ->
                { element = state.element |> mapAtIndex state.folded
                , folded = state.folded + 1
                }
            )
            >> .mapped

    [ 'h', 'i', 'y', 'o' ]
        |> mapIndexed Up Tuple.pair
    --> [ ( 0, 'h' ), ( 1, 'i' ), ( 2, 'y' ), ( 3, 'o' ) ]

    [ 'h', 'i', 'y', 'o' ]
        |> mapIndexed Down Tuple.pair
    --> [ ( 3, 'h' ), ( 2, 'i' ), ( 1, 'y' ), ( 0, 'o' ) ]

-}
mapFoldFrom :
    accumulationValue
    -> Direction
    ->
        ({ element : element, folded : accumulationValue }
         -> { element : mappedElement, folded : accumulationValue }
        )
    ->
        (List element
         -> { mapped : List mappedElement, folded : accumulationValue }
        )
mapFoldFrom accumulationValueInitial direction reduce =
    \list ->
        let
            mapFolded : { mapped : List mappedElement, folded : accumulationValue }
            mapFolded =
                list
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
        { mapped = mapFolded.mapped |> mappedOrder
        , folded = mapFolded.folded
        }


{-| Split into equal-sized `chunks` of a given length in a given [`Direction`](Linear#Direction)
The left over elements to one side are in `remainder`

    import Linear exposing (Direction(..))

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> List.Linear.toChunksOf Up 3
    --> { chunks = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
    --> , remainder = [ 7 ]
    --> }

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> List.Linear.toChunksOf Down 3
    --> { remainder = [ 1 ]
    --> , chunks = [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
    --> }

-}
toChunksOf :
    Direction
    -> Int
    ->
        (List element
         ->
            { chunks : List (List element)
            , remainder : List element
            }
        )
toChunksOf chunkingDirection chunkLength =
    let
        toChunksUp () =
            \list ->
                if (list |> List.length) >= chunkLength then
                    let
                        after =
                            list
                                |> List.drop chunkLength
                                |> toChunksUp ()
                    in
                    { chunks =
                        (list |> List.take chunkLength)
                            :: after.chunks
                    , remainder = after.remainder
                    }

                else
                    { chunks = [], remainder = list }
    in
    -- I think we can optimize this further for Down
    \listToChunk ->
        let
            { chunks, remainder } =
                listToChunk
                    |> order chunkingDirection
                    |> toChunksUp ()
        in
        { chunks =
            chunks
                |> order chunkingDirection
                |> List.map (order chunkingDirection)
        , remainder = remainder |> order chunkingDirection
        }


{-| A given number of elements from one side in a [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))

    [ 1, 2, 3, 4 ]
        |> List.Linear.take Up 2
    --> [ 1, 2 ]

    [ 1, 2, 3, 4 ]
        |> List.Linear.take Down 2
    --> [ 3, 4 ]

`[]` if the amount of elements to take is negative

    import Linear exposing (Direction(..))

    [ 1, 2, 3 ]
        |> List.Linear.take Up -100
    --> []

-}
take : Direction -> Int -> (List element -> List element)
take directionToTakeFrom lengthToTake =
    case directionToTakeFrom of
        Up ->
            \list -> list |> List.take lengthToTake

        Down ->
            \list ->
                list
                    |> List.drop ((list |> List.length) - lengthToTake)


{-| Remove a given number of elements from one side in a [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))

    removeFirst =
        List.Linear.drop Up 0

    removeLast =
        List.Linear.drop Down 0

Nothing is dropped if the amount of elements to drop is negative

    import Linear exposing (Direction(..))

    [ 1, 2, 3 ]
        |> List.Linear.drop Up -1
    --> [ 1, 2, 3 ]

-}
drop : Direction -> Int -> (List element -> List element)
drop directionToDropFrom lengthToDrop =
    case directionToDropFrom of
        Up ->
            \list -> list |> List.drop lengthToDrop

        Down ->
            \list ->
                list
                    |> List.take ((list |> List.length) - lengthToDrop)


{-| `Just` the element at the given index in a [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.element ( Down, 0 )
    --> Just 3

    [ 0, 1, 2, 3 ]
        |> List.Linear.element ( Up, 2 )
    --> Just 2

`Nothing` if the index is out of range:

    import Linear exposing (Direction(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.element ( Up, 5 )
    --> Nothing

    [ 0, 1, 2, 3 ]
        |> List.Linear.element ( Up, -1 )
    --> Nothing

If you're using at-operations often, consider using an `Array` instead of a `List`
to get `O(log n)` vs. `O(n)` random access performance

-}
element :
    ( Direction, Int )
    ->
        (List element
         -> Maybe element
        )
element ( direction, index ) =
    if index <= -1 then
        \_ -> Nothing

    else
        \list ->
            let
                length =
                    list |> List.length

                beforeIndexLength =
                    case direction of
                        Up ->
                            index

                        Down ->
                            (length - 1) - index
            in
            if beforeIndexLength <= -1 then
                Nothing

            else
                list |> List.drop beforeIndexLength |> List.head


{-| Alter the element at the given index in a [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))

    [ 1, 2, 2 ]
        |> List.Linear.elementAlter ( Down, 0 )
            (\n -> n + 1)
    --> [ 1, 2, 3 ]

Do nothing if the index is out of range

    import Linear exposing (Direction(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.elementAlter ( Up, 4 )
            (\n -> n + 1)
    --> [ 0, 1, 2, 3 ]

    [ 0, 1, 2, 3 ]
        |> List.Linear.elementAlter ( Up, -1 )
            (\n -> n + 1)
    --> [ 0, 1, 2, 3 ]

If you're using at-operations often, consider using an `Array` instead of a `List`
to get `O(log n)` vs. `O(n)` random access performance

-}
elementAlter :
    ( Direction, Int )
    -> (element -> element)
    ->
        (List element
         -> List element
        )
elementAlter ( direction, index ) elementAtLocationAlter =
    \list ->
        let
            indexUp =
                case direction of
                    Up ->
                        index

                    Down ->
                        (list |> List.length) - 1 - index
        in
        if indexUp >= 0 then
            case list |> List.drop indexUp of
                [] ->
                    list

                elementAtIndex :: beyondIndex ->
                    (list |> List.take indexUp)
                        ++ ((elementAtIndex |> elementAtLocationAlter) :: beyondIndex)

        else
            list


{-| Keep the order if `Up`, reverse if `Down`

    import Linear exposing (Direction(..))

    [ 1, 2, 3 ] |> List.order Down
    --→ [ 3, 2, 1 ]

    [ 1, 2, 3 ] |> List.order Up
    --→ [ 1, 2, 3 ]

**Shouldn't be exposed**

-}
order : Direction -> (List element -> List element)
order direction =
    case direction of
        Up ->
            identity

        Down ->
            List.reverse
