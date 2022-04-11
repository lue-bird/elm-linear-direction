module List.Linear exposing
    ( at
    , foldFrom
    , alter
    , take, drop
    , toChunks
    , access
    )

{-| `List` operations that can be applied in either direction.


## scan

@docs at


## transform

@docs foldFrom

@docs alter


### part

@docs take, drop
@docs toChunks


## deprecated

@docs access

-}

import Linear exposing (DirectionLinear(..), ExpectedIndexInRange(..))


{-| Reduce a `List` in a direction.

    import Linear exposing (DirectionLinear(..))

    [ 'l', 'i', 'v', 'e' ]
        |> List.Linear.foldFrom ( "", Up, String.cons )
    --> "evil"

    [ 'l', 'i', 'v', 'e' ]
        |> List.Linear.foldFrom ( "", Down, String.cons )
    --> "live"

-}
foldFrom :
    ( accumulationValue
    , DirectionLinear
    , element -> accumulationValue -> accumulationValue
    )
    -> List element
    -> accumulationValue
foldFrom ( accumulationValueInitial, direction, reduce ) =
    let
        fold =
            case direction of
                Up ->
                    List.foldl

                Down ->
                    List.foldr
    in
    fold reduce accumulationValueInitial


{-| Split the `List` into equal-`length` `chunks`.
The left over elements to one side are in `remainder`.

    import Linear exposing (DirectionLinear(..))

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> List.Linear.toChunks
            { length = 3, remainder = Up }
    --> { chunks = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
    --> , remainder = [ 7 ]
    --> }

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> List.Linear.toChunks
            { length = 3, remainder = Down }
    --> { remainder = [ 1 ]
    --> , chunks = [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
    --> }

-}
toChunks :
    { length : Int
    , remainder : DirectionLinear
    }
    -> List element
    ->
        { chunks : List (List element)
        , remainder : List element
        }
toChunks chunking listToChunk =
    let
        toChunksUp () =
            \list ->
                if (list |> List.length) >= chunking.length then
                    let
                        after =
                            list
                                |> List.drop chunking.length
                                |> toChunksUp ()
                    in
                    { chunks =
                        (list |> List.take chunking.length)
                            :: after.chunks
                    , remainder = after.remainder
                    }

                else
                    { chunks = [], remainder = list }

        direction =
            chunking.remainder

        { chunks, remainder } =
            listToChunk
                |> order direction
                |> toChunksUp ()
    in
    { chunks =
        chunks
            |> order direction
            |> List.map (order direction)
    , remainder = remainder |> order direction
    }


{-| A given number of elements from one side.

    import Linear exposing (DirectionLinear(..))

    [ 1, 2, 3, 4 ]
        |> List.Linear.take ( Up,2 )
    --> [ 1, 2 ]

    [ 1, 2, 3, 4 ]
        |> List.Linear.take ( Down, 2 )
    --> [ 3, 4 ]

`[]` if the amount of elements to take is negative.

    import Linear exposing (DirectionLinear(..))

    [ 1, 2, 3 ]
        |> List.Linear.take ( Up, -100 )
    --> []

-}
take : ( DirectionLinear, Int ) -> List element -> List element
take ( direction, amount ) =
    case direction of
        Up ->
            List.take amount

        Down ->
            \list ->
                list
                    |> List.drop ((list |> List.length) - amount)


{-| Remove a given number of elements from one side.

    import Linear exposing (DirectionLinear(..))

    removeFirst =
        List.Linear.drop ( Up, 1 )

    removeLast =
        List.Linear.drop ( Down, 1 )

Nothing is dropped if the amount of elements to drop is negative.

    import Linear exposing (DirectionLinear(..))

    [ 1, 2, 3 ]
        |> List.Linear.drop ( Up, -1 )
    --> [ 1, 2, 3 ]

-}
drop : ( DirectionLinear, Int ) -> List element -> List element
drop ( direction, amount ) =
    case direction of
        Up ->
            List.drop amount

        Down ->
            \list ->
                list
                    |> List.take ((list |> List.length) - amount)


{-|

  - @deprecated

    Removed with the next major version

    → call [`Array.Linear.at`](#at) directly

`Just` the element at the given index in the list in a [direction](Linear#DirectionLinear):

    import Linear exposing (DirectionLinear(..))

    [ 0, 1, 2, 3 ]
        |> Linear.at ( Down, 0 )
        |> List.Linear.access
    --> Ok 3

    [ 0, 1, 2, 3 ]
        |> Linear.at ( Up, 2 )
        |> List.Linear.access
    --> Ok 2

`Err` if the index is out of range:

    import Linear exposing (DirectionLinear(..), ExpectedIndexInRange(..))

    [ 0, 1, 2, 3 ]
        |> Linear.at ( Up, 5 )
        |> List.Linear.access
    --> Err (ExpectedIndexForLength 4)

    [ 0, 1, 2, 3 ]
        |> Linear.at ( Up, -1 )
        |> List.Linear.access
    --> Err (ExpectedIndexForLength 4)

If you're using [`at`](Linear#at)-operations often, consider using an `Array` instead of a `List`
to get `O(log n)` vs. `O(n)` random access performance.

-}
access :
    { structure : List element
    , location : ( DirectionLinear, Int )
    }
    -> Result ExpectedIndexInRange element
access =
    \list ->
        list.structure |> at list.location


{-| `Just` the element at the given index in the list in a [direction](Linear#DirectionLinear):

    import Linear exposing (DirectionLinear(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.at ( Down, 0 )
    --> Ok 3

    [ 0, 1, 2, 3 ]
        |> List.Linear.at ( Up, 2 )
    --> Ok 2

`Err` if the index is out of range:

    import Linear exposing (DirectionLinear(..), ExpectedIndexInRange(..))

    [ 0, 1, 2, 3 ]
        |> List.Linear.at ( Up, 5 )
    --> Err (ExpectedIndexForLength 4)

    [ 0, 1, 2, 3 ]
        |> List.Linear.at ( Up, -1 )
    --> Err (ExpectedIndexForLength 4)

If you're using at-operations often, consider using an `Array` instead of a `List`
to get `O(log n)` vs. `O(n)` random access performance.

-}
at :
    ( DirectionLinear, Int )
    -> List element
    -> Result ExpectedIndexInRange element
at ( direction, index ) =
    \list ->
        let
            wholeLength =
                list |> List.length

            beforeLength =
                case direction of
                    Up ->
                        index

                    Down ->
                        wholeLength - 1 - index
        in
        if beforeLength >= 0 then
            case list |> List.drop beforeLength of
                found :: _ ->
                    found |> Ok

                [] ->
                    ExpectedIndexForLength wholeLength |> Err

        else
            ExpectedIndexForLength wholeLength |> Err


{-| Alter the element at the given index in a [direction](Linear#DirectionLinear):

    import Linear exposing (DirectionLinear(..))

    [ 1, 2, 2 ]
        |> Linear.at ( Down, 0 )
        |> List.Linear.alter (\n -> n + 1)
    --> [ 1, 2, 3 ]

Do nothing if the index is out of range:

    import Linear exposing (DirectionLinear(..))

    [ 0, 1, 2, 3 ]
        |> Linear.at ( Up, 4 )
        |> List.Linear.alter (\_ -> 123)
    --> [ 0, 1, 2, 3 ]

    [ 0, 1, 2, 3 ]
        |> Linear.at ( Up, -1 )
        |> List.Linear.alter (\_ -> 123)
    --> [ 0, 1, 2, 3 ]

If you're using at-operations often, consider using an `Array` instead of a `List`
to get `O(log n)` vs. `O(n)` random access performance.

-}
alter :
    (element -> element)
    ->
        { structure : List element
        , location : ( DirectionLinear, Int )
        }
    -> List element
alter elementAlter =
    \list ->
        let
            ( direction, indexInDirection ) =
                list.location

            indexUp =
                case direction of
                    Up ->
                        indexInDirection

                    Down ->
                        (list.structure |> List.length) - 1 - indexInDirection
        in
        if indexUp >= 0 then
            case list.structure |> List.drop indexUp of
                [] ->
                    list.structure

                elementAtIndex :: beyondIndex ->
                    (list.structure |> List.take indexUp)
                        ++ ((elementAtIndex |> elementAlter) :: beyondIndex)

        else
            list.structure


{-| Keep the order if `Up`, reverse if `Down`.

    import Linear exposing (DirectionLinear(..))

    [ 1, 2, 3 ] |> List.order Down
    --→ [ 3, 2, 1 ]

    [ 1, 2, 3 ] |> List.order Up
    --→ [ 1, 2, 3 ]

**Shouldn't be exposed**

-}
order : DirectionLinear -> List element -> List element
order direction =
    case direction of
        Up ->
            identity

        Down ->
            List.reverse
