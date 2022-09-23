module Tests exposing (suite)

import Array exposing (Array)
import Array.Linear
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Linear exposing (Direction(..), IndexIntOutOfRange(..))
import List.Linear
import Random
import Test exposing (Test, test)


suite : Test
suite =
    Test.describe "linear-direction"
        [ arrayTests
        , listTests
        ]


listTests : Test
listTests =
    Test.describe "list"
        [ Test.describe "toChunksOf"
            [ test "Up"
                (\() ->
                    [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> List.Linear.toChunksOf Up 3
                        |> Expect.equal
                            { chunks = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
                            , remainder = [ 7 ]
                            }
                )
            , test "Down"
                (\() ->
                    [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> List.Linear.toChunksOf Down 3
                        |> Expect.equal
                            { remainder = [ 1 ]
                            , chunks = [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
                            }
                )
            ]
        , Test.describe "take"
            [ Test.fuzz
                (Fuzz.list Fuzz.int)
                "Up"
                (\after ->
                    [ 123456, 1234 ]
                        ++ after
                        |> List.Linear.take ( Up, 2 )
                        |> Expect.equalLists
                            [ 123456, 1234 ]
                )
            , Test.fuzz
                (Fuzz.list Fuzz.int)
                "Down"
                (\before ->
                    before
                        ++ [ 123456, 1234 ]
                        |> List.Linear.take ( Down, 2 )
                        |> Expect.equalLists
                            [ 123456, 1234 ]
                )
            ]
        , Test.describe "drop"
            [ Test.fuzz
                (Fuzz.list Fuzz.int)
                "Up"
                (\list ->
                    list
                        ++ [ 123456, 1234 ]
                        |> List.Linear.drop ( Up, list |> List.length )
                        |> Expect.equalLists
                            [ 123456, 1234 ]
                )
            , Test.fuzz
                (Fuzz.list Fuzz.int)
                "Down"
                (\list ->
                    [ 123456, 1234 ]
                        ++ list
                        |> List.Linear.drop ( Down, list |> List.length )
                        |> Expect.equalLists
                            [ 123456, 1234 ]
                )
            ]
        , Test.describe "element"
            [ Test.fuzz
                (Fuzz.constant
                    (\list direction -> { list = list, direction = direction })
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
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
                        |> Expect.equalLists
                            (List.range 0 ((list |> List.length) - 1)
                                |> reverseIfLastToFirst
                                |> List.map
                                    (\i ->
                                        list |> List.Linear.element ( direction, i )
                                    )
                            )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\list direction above ->
                        { list = list, direction = direction, above = above }
                    )
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index >= length"
                (\{ list, direction, above } ->
                    list
                        |> List.Linear.element
                            ( direction, (list |> List.length) + above )
                        |> Expect.equal
                            (Err IndexIntBeyondElements)
                )
            , Test.fuzz
                (Fuzz.constant
                    (\list direction index ->
                        { list = list, direction = direction, index = index }
                    )
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative → Err IndexIntNegative"
                (\{ list, direction, index } ->
                    list
                        |> List.Linear.element ( direction, index )
                        |> Expect.equal
                            (Err IndexIntNegative)
                )
            ]
        , Test.describe "alter"
            [ Test.describe "index in range"
                [ test "Down"
                    (\() ->
                        [ 0, 1, 2, 3 ]
                            |> List.Linear.elementAlter ( Down, 0 )
                                (\n -> n + 100)
                            |> Expect.equalLists [ 0, 1, 2, 103 ]
                    )
                , test "Up"
                    (\() ->
                        [ 0, 1, 2, 3 ]
                            |> List.Linear.elementAlter ( Up, 2 )
                                (\n -> n + 100)
                            |> Expect.equalLists [ 0, 1, 102, 3 ]
                    )
                ]
            , Test.fuzz
                (Fuzz.constant
                    (\list direction -> { list = list, direction = direction })
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "index >= length → identity"
                (\{ list, direction } ->
                    list
                        |> List.Linear.elementAlter
                            ( direction, list |> List.length )
                            (\n -> n + 100)
                        |> Expect.equalLists
                            list
                )
            , Test.fuzz
                (Fuzz.constant
                    (\list direction index ->
                        { list = list, direction = direction, index = index }
                    )
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative → identity"
                (\{ list, direction, index } ->
                    list
                        |> List.Linear.elementAlter ( direction, index )
                            (\n -> n + 100)
                        |> Expect.equalLists
                            list
                )
            ]
        ]


arrayTests : Test
arrayTests =
    Test.describe "array"
        [ Test.describe "element"
            [ Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "valid index"
                (\{ array, direction } ->
                    let
                        length =
                            array |> Array.length

                        toUp index =
                            case direction of
                                Up ->
                                    index

                                Down ->
                                    length - 1 - index
                    in
                    array
                        |> Array.map Ok
                        |> expectEqualArrays
                            (Array.initialize
                                length
                                (\i ->
                                    array |> Array.Linear.element ( direction, i |> toUp )
                                )
                            )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "index >= length"
                (\{ array, direction } ->
                    let
                        length =
                            array |> Array.length
                    in
                    array
                        |> Array.Linear.element ( direction, length )
                        |> Expect.equal
                            (Err IndexIntBeyondElements)
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction index ->
                        { array = array, direction = direction, index = index }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative"
                (\{ array, direction, index } ->
                    array
                        |> Array.Linear.element ( direction, index )
                        |> Expect.equal
                            (Err IndexIntNegative)
                )
            ]
        , Test.describe "elementReplace"
            [ test "valid index Up → sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.elementReplace ( Up, 2 )
                            (\() -> -3)
                        |> expectEqualArrays
                            (Array.fromList [ 1, 2, -3, 4 ])
                )
            , test "valid index Down → sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.elementReplace ( Down, 1 )
                            (\() -> -3)
                        |> expectEqualArrays
                            (Array.fromList [ 1, 2, -3, 4 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction index ->
                        { array = array, direction = direction, index = index }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative → changes nothing"
                (\{ array, direction, index } ->
                    array
                        |> Array.Linear.elementReplace ( direction, index )
                            (\() -> 123)
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "index >= length changes nothing"
                (\{ array, direction } ->
                    array
                        |> Array.Linear.elementReplace
                            ( direction, array |> Array.length )
                            (\() -> 123)
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "insert"
            [ Test.describe "at valid index"
                [ test "Up"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insert ( Up, 2 )
                                (\() -> 123)
                            |> expectEqualArrays
                                (Array.fromList [ 1, 2, 123, 3, 4 ])
                    )
                , test "Down"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insert ( Down, 2 )
                                (\() -> 123)
                            |> expectEqualArrays
                                (Array.fromList [ 1, 2, 123, 3, 4 ])
                    )
                , Test.describe "at index = length"
                    [ Test.fuzz
                        (Fuzz.array Fuzz.int)
                        "Up"
                        (\array ->
                            array
                                |> Array.Linear.insert
                                    ( Up, array |> Array.length )
                                    (\() -> 123)
                                |> expectEqualArrays
                                    (array |> Array.push 123)
                        )
                    , Test.fuzz
                        (Fuzz.list Fuzz.int)
                        "Down"
                        (\list ->
                            Array.fromList list
                                |> Array.Linear.insert
                                    ( Down, list |> List.length )
                                    (\() -> 123)
                                |> expectEqualArrays
                                    (123 :: list |> Array.fromList)
                        )
                    ]
                ]
            , Test.fuzz
                (Fuzz.constant
                    (\array direction index ->
                        { array = array, direction = direction, index = index }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative → no change"
                (\{ array, direction, index } ->
                    array
                        |> Array.Linear.insert ( direction, index )
                            (\() -> 123)
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "index > length → no change"
                (\{ array, direction } ->
                    array
                        |> Array.Linear.insert
                            ( direction, (array |> Array.length) + 1 )
                            (\() -> 123)
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "squeezeIn"
            [ Test.describe "valid index"
                [ test "Up"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Array.Linear.squeezeIn ( Up, 1 )
                                (\() -> Array.fromList [ 'b', 'c' ])
                            |> expectEqualArrays
                                (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                    )
                , test "Down"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Array.Linear.squeezeIn ( Down, 2 )
                                (\() -> Array.fromList [ 'b', 'c' ])
                            |> expectEqualArrays
                                (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                    )
                ]
            , Test.fuzz
                (Fuzz.constant
                    (\array direction index ->
                        { array = array, direction = direction, index = index }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative → no change"
                (\{ array, direction, index } ->
                    array
                        |> Array.Linear.squeezeIn
                            ( direction, index )
                            (\() -> Array.fromList [ -2, -1 ])
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "index > length → no change"
                (\{ array, direction } ->
                    array
                        |> Array.Linear.squeezeIn
                            ( direction, (array |> Array.length) + 1 )
                            (\() -> Array.fromList [ -2, -1 ])
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "elementRemove"
            [ test "Up → removes the element at the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.elementRemove
                            ( Up, 2 )
                        |> expectEqualArrays
                            (Array.fromList [ 1, 2, 4 ])
                )
            , test "Down → removes the element at the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.elementRemove
                            ( Down, 1 )
                        |> expectEqualArrays
                            (Array.fromList [ 1, 2, 4 ])
                )
            ]
        , Test.describe
            "elementAlter"
            [ Test.describe
                "valid index"
                [ test "Up"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.elementAlter ( Up, 2 ) negate
                            |> expectEqualArrays
                                (Array.fromList [ 1, 2, -3, 4 ])
                    )
                ]
            , test
                "Down"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.elementAlter ( Down, 1 ) negate
                        |> expectEqualArrays
                            (Array.fromList [ 1, 2, -3, 4 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction index ->
                        { array = array, direction = direction, index = index }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative"
                (\{ array, direction, index } ->
                    array
                        |> Array.Linear.elementAlter ( direction, index ) negate
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction above ->
                        { array = array, direction = direction, above = above }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange 1 Random.maxInt)
                )
                "too high index"
                (\{ array, direction, above } ->
                    array
                        |> Array.Linear.elementAlter
                            ( direction, (array |> Array.length) + above )
                            negate
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "take"
            [ Test.fuzz
                (Fuzz.constant
                    (\array extension direction ->
                        { array = array, extension = extension, direction = direction }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "glue-ing anything, then taking the original length → identity"
                (\{ extension, direction, array } ->
                    array
                        |> Array.Linear.glue direction extension
                        |> Array.Linear.take ( direction, array |> Array.length )
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "drop"
            [ Test.fuzz
                (Fuzz.constant
                    (\array extension direction ->
                        { array = array, extension = extension, direction = direction }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "glue-ing anything, then dropping the extension length → identity"
                (\{ extension, direction, array } ->
                    array
                        |> Array.Linear.glue direction extension
                        |> Array.Linear.drop
                            ( direction |> Linear.opposite
                            , extension |> Array.length
                            )
                        |> expectEqualArrays
                            array
                )
            , Test.describe "hardcoded"
                [ test "Up"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.drop ( Up, 1 )
                            |> expectEqualArrays
                                (Array.fromList [ 2, 3, 4 ])
                    )
                , test "Down"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.drop ( Down, 1 )
                            |> expectEqualArrays
                                (Array.fromList [ 1, 2, 3 ])
                    )
                ]
            ]
        , Test.describe "glue"
            [ Test.fuzz
                (Fuzz.constant
                    (\array extension ->
                        { array = array, extension = extension }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                )
                "Up"
                (\{ array, extension } ->
                    array
                        |> Array.Linear.glue Up extension
                        |> expectEqualArrays
                            (Array.append array extension)
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array extension ->
                        { array = array, extension = extension }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                )
                "Down"
                (\{ array, extension } ->
                    array
                        |> Array.Linear.glue Down extension
                        |> expectEqualArrays
                            (Array.append extension array)
                )
            ]
        , Test.describe "toChunksOf"
            [ test "Up"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> Array.Linear.toChunksOf Up 3
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
                        |> Array.Linear.toChunksOf Down 3
                        |> Expect.equal
                            { remainder = Array.fromList [ 1 ]
                            , chunks =
                                [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            }
                )
            ]
        , Test.describe "padToLength"
            [ Test.describe "lengthMinimum > current length"
                [ Test.fuzz
                    (Fuzz.array Fuzz.int)
                    "Up"
                    (\array ->
                        array
                            |> Array.Linear.padToLength Up
                                (\l -> Array.repeat l 0)
                                ((array |> Array.length) + 2)
                            |> expectEqualArrays
                                (Array.append array (Array.repeat 2 0))
                    )
                , Test.fuzz
                    (Fuzz.array Fuzz.int)
                    "Down"
                    (\array ->
                        array
                            |> Array.Linear.padToLength Down
                                (\l -> Array.repeat l 0)
                                ((array |> Array.length) + 2)
                            |> expectEqualArrays
                                (Array.append (Array.repeat 2 0) array)
                    )
                ]
            , Test.fuzz
                (Fuzz.constant
                    (\array direction -> { array = array, direction = direction })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                )
                "lengthMinimum < current length  → identity"
                (\{ array, direction } ->
                    let
                        lengthShortened =
                            (array |> Array.length) - 2
                    in
                    array
                        |> Array.Linear.padToLength direction
                            (\l -> Array.repeat l 0)
                            lengthShortened
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array direction length ->
                        { array = array, direction = direction, length = length }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap linearDirectionFuzz
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "lengthMinimum negative → identity"
                (\{ array, direction, length } ->
                    array
                        |> Array.Linear.padToLength direction
                            (\l -> Array.repeat l 0)
                            length
                        |> expectEqualArrays
                            array
                )
            ]
        ]



-- help


expectEqualArrays : Array element -> (Array element -> Expectation)
expectEqualArrays expectedArray =
    \actualArray ->
        actualArray
            |> Array.toList
            |> Expect.equalLists
                (expectedArray |> Array.toList)


linearDirectionFuzz : Fuzzer Direction
linearDirectionFuzz =
    Fuzz.oneOf (List.map Fuzz.constant [ Up, Down ])
