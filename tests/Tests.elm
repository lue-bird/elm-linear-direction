module Tests exposing (suite)

import Array
import Array.Linear
import Expect
import Fuzz exposing (Fuzzer)
import Linear exposing (DirectionLinear(..), ExpectedIndexInRange(..))
import List.Linear
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "linear-direction"
        [ arrayTests
        , listTests
        ]


listTests : Test
listTests =
    describe "list"
        [ describe "toChunks"
            [ test "Up"
                (\() ->
                    [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> List.Linear.toChunks
                            { length = 3, remainder = Up }
                        |> Expect.equal
                            { chunks = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
                            , remainder = [ 7 ]
                            }
                )
            , test "Down"
                (\() ->
                    [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> List.Linear.toChunks
                            { length = 3, remainder = Down }
                        |> Expect.equal
                            { remainder = [ 1 ]
                            , chunks = [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
                            }
                )
            ]
        , describe "take"
            [ test "Up"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.Linear.take ( Up, 2 )
                        |> Expect.equalLists [ 1, 2 ]
                )
            , test "Down"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.Linear.take ( Down, 2 )
                        |> Expect.equalLists [ 4, 5 ]
                )
            ]
        , describe "drop"
            [ test "Up"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.Linear.drop ( Up, 2 )
                        |> Expect.equalLists [ 3, 4, 5 ]
                )
            , test "Down"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.Linear.drop ( Down, 2 )
                        |> Expect.equalLists [ 1, 2, 3 ]
                )
            ]
        , describe "at"
            [ Test.fuzz
                (Fuzz.constant
                    (\list direction -> { list = list, direction = direction })
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "valid index"
                (\{ list, direction } ->
                    let
                        reverseIfLastToFirst =
                            case direction of
                                Up ->
                                    identity

                                Down ->
                                    List.reverse
                    in
                    list
                        |> List.map Ok
                        |> Expect.equal
                            (List.range 0 ((list |> List.length) - 1)
                                |> reverseIfLastToFirst
                                |> List.map
                                    (\i ->
                                        list |> List.Linear.at ( direction, i )
                                    )
                            )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\list direction -> { list = list, direction = direction })
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "index >= length"
                (\{ list, direction } ->
                    let
                        length =
                            list |> List.length
                    in
                    list
                        |> List.Linear.at ( direction, length )
                        |> Expect.equal (Err (ExpectedIndexForLength length))
                )
            , Test.fuzz
                (Fuzz.constant
                    (\list direction -> { list = list, direction = direction })
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "negative index → Nothing"
                (\{ list, direction } ->
                    list
                        |> List.Linear.at ( direction, -1 )
                        |> Expect.equal
                            (Err (ExpectedIndexForLength (list |> List.length)))
                )
            ]
        , describe "alter"
            [ describe "at index in range"
                [ test "Down"
                    (\() ->
                        [ 0, 1, 2, 3 ]
                            |> Linear.at ( Down, 0 )
                            |> List.Linear.alter (\n -> n + 100)
                            |> Expect.equalLists [ 0, 1, 2, 103 ]
                    )
                , test "Up"
                    (\() ->
                        [ 0, 1, 2, 3 ]
                            |> Linear.at ( Up, 2 )
                            |> List.Linear.alter (\n -> n + 100)
                            |> Expect.equalLists [ 0, 1, 102, 3 ]
                    )
                ]
            , Test.fuzz
                (Fuzz.constant
                    (\list direction -> { list = list, direction = direction })
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "at index >= length → identity"
                (\{ list, direction } ->
                    list
                        |> Linear.at ( direction, list |> List.length )
                        |> List.Linear.alter (\n -> n + 100)
                        |> Expect.equalLists list
                )
            , Test.fuzz
                (Fuzz.constant
                    (\list direction -> { list = list, direction = direction })
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "at index negative → identity"
                (\{ list, direction } ->
                    list
                        |> Linear.at ( direction, -1 )
                        |> List.Linear.alter (\n -> n + 100)
                        |> Expect.equalLists list
                )
            ]
        ]


arrayTests : Test
arrayTests =
    describe "array"
        [ describe "at"
            [ Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "valid index"
                (\{ array, direction } ->
                    let
                        toUp index =
                            case direction of
                                Up ->
                                    index

                                Down ->
                                    (array |> Array.length) - 1 - index
                    in
                    array
                        |> Array.map Ok
                        |> Expect.equal
                            (Array.initialize
                                (array |> Array.length)
                                (\i ->
                                    array |> Array.Linear.at ( direction, i |> toUp )
                                )
                            )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "index >= length"
                (\{ array, direction } ->
                    let
                        length =
                            array |> Array.length
                    in
                    array
                        |> Array.Linear.at ( direction, length )
                        |> Expect.equal
                            (Err (ExpectedIndexForLength length))
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "index negative"
                (\{ array, direction } ->
                    array
                        |> Array.Linear.at ( direction, -1 )
                        |> Expect.equal
                            (Err (ExpectedIndexForLength (array |> Array.length)))
                )
            ]
        , describe "replaceWith"
            [ test "valid index Up → sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Linear.at ( Up, 2 )
                        |> Array.Linear.replaceWith (\() -> -3)
                        |> Expect.equal
                            (Array.fromList [ 1, 2, -3, 4 ])
                )
            , test "valid index Down → sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Linear.at ( Down, 1 )
                        |> Array.Linear.replaceWith (\() -> -3)
                        |> Expect.equal
                            (Array.fromList [ 1, 2, -3, 4 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "negative index → changes nothing"
                (\{ array, direction } ->
                    array
                        |> Linear.at ( direction, -1 )
                        |> Array.Linear.replaceWith (\() -> 123)
                        |> Expect.equal array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "index >= length changes nothing"
                (\{ array, direction } ->
                    array
                        |> Linear.at
                            ( direction, array |> Array.length )
                        |> Array.Linear.replaceWith (\() -> 123)
                        |> Expect.equal array
                )
            ]
        , describe "insert"
            [ describe "at valid index"
                [ test "Up"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Linear.at ( Up, 2 )
                            |> Array.Linear.insert (\() -> 123)
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 123, 3, 4 ])
                    )
                , test "Down"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Linear.at ( Down, 2 )
                            |> Array.Linear.insert (\() -> 123)
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 123, 3, 4 ])
                    )
                , describe "at index = length"
                    [ Test.fuzz
                        (Fuzz.array Fuzz.int)
                        "Up"
                        (\array ->
                            array
                                |> Linear.at ( Up, array |> Array.length )
                                |> Array.Linear.insert (\() -> 123)
                                |> Expect.equal
                                    (array |> Array.push 123)
                        )
                    , Test.fuzz
                        (Fuzz.list Fuzz.int)
                        "Down"
                        (\list ->
                            Array.fromList list
                                |> Linear.at ( Down, list |> List.length )
                                |> Array.Linear.insert (\() -> 123)
                                |> Expect.equal
                                    (123 :: list |> Array.fromList)
                        )
                    ]
                ]
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "negative index → no change"
                (\{ array, direction } ->
                    array
                        |> Linear.at ( direction, -1 )
                        |> Array.Linear.insert (\() -> 123)
                        |> Expect.equal array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "index > length → no change"
                (\{ array, direction } ->
                    array
                        |> Linear.at
                            ( direction, (array |> Array.length) + 1 )
                        |> Array.Linear.insert (\() -> 123)
                        |> Expect.equal array
                )
            ]
        , describe "squeezeInAt"
            [ describe "valid index"
                [ test "Up"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Linear.at ( Up, 1 )
                            |> Array.Linear.squeezeIn
                                (\() -> Array.fromList [ 'b', 'c' ])
                            |> Expect.equal
                                (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                    )
                , test "Down"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Linear.at ( Down, 2 )
                            |> Array.Linear.squeezeIn
                                (\() -> Array.fromList [ 'b', 'c' ])
                            |> Expect.equal
                                (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                    )
                ]
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "negative index → no change"
                (\{ array, direction } ->
                    array
                        |> Linear.at ( direction, -1 )
                        |> Array.Linear.squeezeIn
                            (\() -> Array.fromList [ -2, -1 ])
                        |> Expect.equal array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "index > length → no change"
                (\{ array, direction } ->
                    array
                        |> Linear.at
                            ( direction, (array |> Array.length) + 1 )
                        |> Array.Linear.squeezeIn
                            (\() -> Array.fromList [ -2, -1 ])
                        |> Expect.equal array
                )
            ]
        , describe "removeAt"
            [ test "Up → removes the element at the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Linear.at ( Up, 2 )
                        |> Array.Linear.remove
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 4 ])
                )
            , test "Down → removes the element at the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Linear.at ( Down, 1 )
                        |> Array.Linear.remove
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 4 ])
                )
            ]
        , describe "take"
            [ test "Up"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.take ( Up, 3 )
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3 ])
                )
            , test "Down"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.take ( Down, 3 )
                        |> Expect.equal
                            (Array.fromList [ 2, 3, 4 ])
                )
            ]
        , describe "drop"
            [ test "Up"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.drop ( Up, 1 )
                        |> Expect.equal
                            (Array.fromList [ 2, 3, 4 ])
                )
            , test "Down"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.drop ( Down, 1 )
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3 ])
                )
            ]
        , describe "toChunks"
            [ test "Up"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> Array.Linear.toChunks
                            { length = 3, remainder = Up }
                        |> Expect.equal
                            { chunks =
                                [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            , remainder = Array.fromList [ 7 ]
                            }
                )
            , test "Down"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> Array.Linear.toChunks
                            { length = 3, remainder = Down }
                        |> Expect.equal
                            { remainder = Array.fromList [ 1 ]
                            , chunks =
                                [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            }
                )
            ]
        , let
            lengthChange =
                2
          in
          describe "padTo"
            [ describe "lengthMinimum > current length"
                [ Test.fuzz
                    (Fuzz.array Fuzz.int)
                    "Up"
                    (\array ->
                        array
                            |> Array.Linear.padTo
                                { lengthMinimum = (array |> Array.length) + lengthChange
                                , pad = ( Up, \n -> Array.repeat n 0 )
                                }
                            |> Expect.equal
                                (Array.append array (Array.repeat lengthChange 0))
                    )
                , Test.fuzz
                    (Fuzz.array Fuzz.int)
                    "Down"
                    (\array ->
                        array
                            |> Array.Linear.padTo
                                { lengthMinimum = (array |> Array.length) + lengthChange
                                , pad = ( Down, \n -> Array.repeat n 0 )
                                }
                            |> Expect.equal
                                (Array.append (Array.repeat lengthChange 0) array)
                    )
                ]
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "lengthMinimum < current length  → identity"
                (\{ array, direction } ->
                    let
                        lengthShortened =
                            (array |> Array.length) - lengthChange
                    in
                    array
                        |> Array.Linear.padTo
                            { lengthMinimum = lengthShortened
                            , pad = ( direction, \n -> Array.repeat n 0 )
                            }
                        |> Expect.equal array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap directionLinearFuzz
                )
                "lengthMinimum negative → identity"
                (\{ array, direction } ->
                    array
                        |> Array.Linear.padTo
                            { lengthMinimum = -1
                            , pad = ( direction, \n -> Array.repeat n 0 )
                            }
                        |> Expect.equal array
                )
            ]
        ]



-- used


directionLinearFuzz : Fuzzer DirectionLinear
directionLinearFuzz =
    Fuzz.oneOf
        [ Fuzz.constant Up
        , Fuzz.constant Down
        ]
