module List.Linear exposing
    ( element
    , foldFrom, foldTrace, foldTraceFrom
    , elementAlter
    , take, drop
    , toChunksOf
    )

{-| `List` operations that can be applied in either [`Direction`](Linear#Direction)


## scan

@docs element


## transform

@docs foldFrom, foldTrace, foldTraceFrom

@docs elementAlter


### part

@docs take, drop
@docs toChunksOf

-}

import Linear exposing (Direction(..), IndexIntOutOfRange(..))


{-| Reduce in a [`Direction`](Linear#Direction)

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


{-| Reduce in a given [`Direction`](Linear#Direction),
stacking up all of the intermediate results

    import Linear exposing (Direction(..))

    [ 1, 2, 3, 4 ]
        |> List.Linear.foldTraceFrom 0 Up (\el soFar -> soFar + el)
    --> [ 1, 3, 6, 10 ]

    [ 1, 2, 3 ]
        |> List.Linear.foldTraceFrom 0 Down (\el soFar -> soFar - el)
    --> [ -3, -5, -6 ]

-}
foldTraceFrom :
    accumulationValue
    -> Direction
    -> (element -> (accumulationValue -> accumulationValue))
    ->
        (List element
         -> List accumulationValue
        )
foldTraceFrom initialAccumulationValue direction accumulate =
    \list ->
        list
            |> foldTraceReverseFrom initialAccumulationValue direction accumulate
            |> List.reverse


foldTraceReverseFrom :
    accumulationValue
    -> Direction
    -> (element -> (accumulationValue -> accumulationValue))
    ->
        (List element
         -> List accumulationValue
        )
foldTraceReverseFrom initialAccumulationValue direction accumulate =
    \list ->
        list
            |> foldFrom
                { accumulated = initialAccumulationValue
                , trail = []
                }
                direction
                (\el soFar ->
                    let
                        accumulatedWithElement =
                            soFar.accumulated |> accumulate el
                    in
                    { accumulated = accumulatedWithElement
                    , trail = soFar.trail |> (::) accumulatedWithElement
                    }
                )
            |> .trail


{-| [`foldTraceFrom`](#foldTraceFrom) its first element
looking in a given [`Direction`](Linear#Direction)

    import Linear exposing (Direction(..))

    [ 1, 2, 3 ]
        |> List.Linear.foldTrace Down (\el soFar -> soFar + el)
    --> [ 3, 5, 6 ]

    [ 1, 2, 3 ]
        |> List.Linear.foldTrace Down (\el soFar -> soFar - el)
    --> [ 3, 1, 0 ]

-}
foldTrace :
    Direction
    -> (element -> (element -> element))
    ->
        (List element
         -> List element
        )
foldTrace direction accumulate =
    \list ->
        list
            |> foldTraceReverse direction accumulate
            |> List.reverse


foldTraceReverse :
    Direction
    -> (element -> (element -> element))
    ->
        (List element
         -> List element
        )
foldTraceReverse direction accumulate =
    case direction of
        Up ->
            \list ->
                case list of
                    [] ->
                        []

                    head :: tail ->
                        tail |> foldTraceFrom head Up accumulate

        Down ->
            \list ->
                case list of
                    [] ->
                        []

                    [ only ] ->
                        [ only ]

                    element0 :: tail ->
                        case tail |> foldTraceReverse Down accumulate of
                            [] ->
                                []

                            (accumulated :: _) as foldTraced ->
                                foldTraced
                                    |> (::) (accumulated |> accumulate element0)


{-| Split into equal-`length` `chunks`.
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
        |> List.Linear.take ( Up,2 )
    --> [ 1, 2 ]

    [ 1, 2, 3, 4 ]
        |> List.Linear.take ( Down, 2 )
    --> [ 3, 4 ]

`[]` if the amount of elements to take is negative.

    import Linear exposing (Direction(..))

    [ 1, 2, 3 ]
        |> List.Linear.take ( Up, -100 )
    --> []

-}
take : ( Direction, Int ) -> (List element -> List element)
take ( directionToTakeFrom, lengthToTake ) =
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
        List.Linear.drop ( Up, 0 )

    removeLast =
        List.Linear.drop ( Down, 0 )

Nothing is dropped if the amount of elements to drop is negative

    import Linear exposing (Direction(..))

    [ 1, 2, 3 ]
        |> List.Linear.drop ( Up, -1 )
    --> [ 1, 2, 3 ]

-}
drop : ( Direction, Int ) -> (List element -> List element)
drop ( directionToDropFrom, lengthToDrop ) =
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
    --> Ok 3

    [ 0, 1, 2, 3 ]
        |> List.Linear.element ( Up, 2 )
    --> Ok 2

`Err` if the index is out of range:

    import Linear exposing (Direction(..), IndexIntOutOfRange(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.element ( Up, 5 )
    --> Err IndexIntBeyondElements

    [ 0, 1, 2, 3 ]
        |> List.Linear.element ( Up, -1 )
    --> Err IndexIntNegative

If you're using at-operations often, consider using an `Array` instead of a `List`
to get `O(log n)` vs. `O(n)` random access performance

-}
element :
    ( Direction, Int )
    ->
        (List element
         -> Result IndexIntOutOfRange element
        )
element ( direction, index ) =
    if index <= -1 then
        \_ -> IndexIntNegative |> Err

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
                IndexIntBeyondElements |> Err

            else
                case list |> List.drop beforeIndexLength of
                    found :: _ ->
                        found |> Ok

                    [] ->
                        IndexIntBeyondElements |> Err


{-| Alter the element at the given index in a [`Direction`](Linear#Direction):

    import Linear exposing (Direction(..))

    [ 1, 2, 2 ]
        |> List.Linear.elementAlter ( Down, 0 )
            (\n -> n + 1)
    --> [ 1, 2, 3 ]

Do nothing if the index is out of range:

    import Linear exposing (Direction(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.elementAlter ( Up, 4 )
            (\_ -> 123)
    --> [ 0, 1, 2, 3 ]

    [ 0, 1, 2, 3 ]
        |> List.Linear.elementAlter ( Up, -1 )
            (\_ -> 123)
    --> [ 0, 1, 2, 3 ]

If you're using at-operations often, consider using an `Array` instead of a `List`
to get `O(log n)` vs. `O(n)` random access performance.

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


{-| Keep the order if `Up`, reverse if `Down`.

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
