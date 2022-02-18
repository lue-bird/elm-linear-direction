module Tests exposing (suite)

import Array exposing (Array)
import Array.Linear
import Expect
import LinearDirection exposing (LinearDirection(..))
import List.Linear
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "elm-linear-direction"
        [ arrayTests
        , listTests
        ]


listTests : Test
listTests =
    describe "list"
        [ describe "toChunksOf"
            [ test "FirstToLast"
                (\() ->
                    [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> List.Linear.toChunksOf 3 FirstToLast
                        |> Expect.equal
                            { chunks = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
                            , remainder = [ 7 ]
                            }
                )
            , test "LastToFirst"
                (\() ->
                    [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> List.Linear.toChunksOf 3 LastToFirst
                        |> Expect.equal
                            { remainder = [ 1 ]
                            , chunks = [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
                            }
                )
            ]
        , describe "takeFrom"
            [ test "FirstToLast"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.Linear.take 2 FirstToLast
                        |> Expect.equal [ 1, 2 ]
                )
            , test "LastToFirst"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.Linear.take 2 LastToFirst
                        |> Expect.equal [ 4, 5 ]
                )
            ]
        , describe "dropFrom"
            [ test "FirstToLast"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.Linear.drop FirstToLast 2
                        |> Expect.equal [ 3, 4, 5 ]
                )
            , test "LastToFirst"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.Linear.drop LastToFirst 2
                        |> Expect.equal [ 1, 2, 3 ]
                )
            ]
        , describe "at"
            [ test "FirstToLast at valid index"
                (\() ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.Linear.at 0 LastToFirst
                        |> Expect.equal (Just 4)
                )
            , test "LastToFirst at valid index"
                (\() ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.Linear.at 2 FirstToLast
                        |> Expect.equal (Just 2)
                )
            , test "FirstToLast index >= length"
                (\() ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.Linear.at 5 FirstToLast
                        |> Expect.equal Nothing
                )
            , test "FirstToLast at negative index"
                (\() ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.Linear.at -1 FirstToLast
                        |> Expect.equal Nothing
                )
            ]
        ]


arrayTests : Test
arrayTests =
    describe "array"
        [ describe "at"
            [ test "FirstToLast at a valid index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Expect.all
                            [ Array.Linear.at 0 FirstToLast >> Expect.equal (Just 1)
                            , Array.Linear.at 1 FirstToLast >> Expect.equal (Just 2)
                            , Array.Linear.at 2 FirstToLast >> Expect.equal (Just 3)
                            , Array.Linear.at 3 FirstToLast >> Expect.equal (Just 4)
                            ]
                )
            , test "FirstToLast at a too high index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.at 100 FirstToLast
                        |> Expect.equal Nothing
                )
            , test "LastToFirst at a too high index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.at 100 LastToFirst
                        |> Expect.equal Nothing
                )
            , test "LastToFirst at a valid index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Expect.all
                            [ Array.Linear.at 0 LastToFirst >> Expect.equal (Just 4)
                            , Array.Linear.at 1 LastToFirst >> Expect.equal (Just 3)
                            , Array.Linear.at 2 LastToFirst >> Expect.equal (Just 2)
                            , Array.Linear.at 3 LastToFirst >> Expect.equal (Just 1)
                            ]
                )
            ]
        , describe "replaceAt"
            [ test "valid index FirstToLast sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.replaceAt 2 FirstToLast -3
                        |> Expect.equal
                            (Array.fromList [ 1, 2, -3, 4 ])
                )
            , test "negative index FirstToLast changes nothing"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.replaceAt -1 FirstToLast 123
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3, 4 ])
                )
            , test "too high index FirstToLast changes nothing"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.replaceAt 100 FirstToLast 123
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3, 4 ])
                )
            , test "valid index LastToFirst sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.replaceAt 1 LastToFirst -3
                        |> Expect.equal
                            (Array.fromList [ 1, 2, -3, 4 ])
                )
            , test "negative index LastToFirst changes nothing"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.replaceAt -1 LastToFirst 123
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3, 4 ])
                )
            , test "too high index LastToFirst changes nothing"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.replaceAt 100 LastToFirst 123
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3, 4 ])
                )
            ]
        , describe "insertAt"
            [ describe "FirstToLast"
                [ test "valid index"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insertAt 2 FirstToLast 123
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 123, 3, 4 ])
                    )
                , test "length"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insertAt 4 FirstToLast 123
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3, 4, 123 ])
                    )
                , test "negative index → no change"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insertAt -1 FirstToLast 123
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3, 4 ])
                    )
                , test "index > length → no change"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insertAt 5 FirstToLast 123
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3, 4 ])
                    )
                ]
            , describe "LastToFirst"
                [ test "valid index"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insertAt 2 LastToFirst 123
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 123, 3, 4 ])
                    )
                , test "length"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insertAt 4 LastToFirst 123
                            |> Expect.equal
                                (Array.fromList [ 123, 1, 2, 3, 4 ])
                    )
                , test "negative index → no change"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insertAt -1 LastToFirst 123
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3, 4 ])
                    )
                , test "index > length → no change"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.Linear.insertAt 5 LastToFirst 123
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3, 4 ])
                    )
                ]
            ]
        , describe "squeezeInAt"
            [ describe "FirstToLast"
                [ test "valid index"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Array.Linear.squeezeInAt 1
                                FirstToLast
                                (Array.fromList [ 'b', 'c' ])
                            |> Expect.equal
                                (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                    )
                , test "negative index → no change"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Array.Linear.squeezeInAt -1
                                FirstToLast
                                (Array.fromList [ 'b', 'c' ])
                            |> Expect.equal
                                (Array.fromList [ 'a', 'd', 'e' ])
                    )
                , test "index > length → no change"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Array.Linear.squeezeInAt 4
                                FirstToLast
                                (Array.fromList [ 'b', 'c' ])
                            |> Expect.equal
                                (Array.fromList [ 'a', 'd', 'e' ])
                    )
                ]
            , describe "LastToFirst"
                [ test "valid index"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Array.Linear.squeezeInAt 2
                                LastToFirst
                                (Array.fromList [ 'b', 'c' ])
                            |> Expect.equal
                                (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                    )
                , test "negative index → no change"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Array.Linear.squeezeInAt -1
                                LastToFirst
                                (Array.fromList [ 'b', 'c' ])
                            |> Expect.equal
                                (Array.fromList [ 'a', 'd', 'e' ])
                    )
                , test "index > length → no change"
                    (\() ->
                        Array.fromList [ 'a', 'd', 'e' ]
                            |> Array.Linear.squeezeInAt 4
                                LastToFirst
                                (Array.fromList [ 'b', 'c' ])
                            |> Expect.equal
                                (Array.fromList [ 'a', 'd', 'e' ])
                    )
                ]
            ]
        , describe "removeAt"
            [ test "removeAt FirstToLast removes the element at the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.removeAt 2 FirstToLast
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 4 ])
                )
            , test "removeAt LastToFirst removes the element at the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.removeAt 1 LastToFirst
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 4 ])
                )
            ]
        , describe "take"
            [ test "take FirstToLast"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.take 3 FirstToLast
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3 ])
                )
            , test "take LastToFirst"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.take 3 LastToFirst
                        |> Expect.equal
                            (Array.fromList [ 2, 3, 4 ])
                )
            ]
        , describe "drop"
            [ test "drop FirstToLast"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.drop 1 FirstToLast
                        |> Expect.equal
                            (Array.fromList [ 2, 3, 4 ])
                )
            , test "drop LastToFirst"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.Linear.drop 1 LastToFirst
                        |> Expect.equal
                            (Array.fromList [ 1, 2, 3 ])
                )
            ]
        , describe "toChunksOf"
            [ test "FirstToLast"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> Array.Linear.toChunksOf 3 FirstToLast
                        |> Expect.equal
                            { chunks =
                                [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            , remainder = Array.fromList [ 7 ]
                            }
                )
            , test "LastToFirst"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> Array.Linear.toChunksOf 3 LastToFirst
                        |> Expect.equal
                            { remainder = Array.fromList [ 1 ]
                            , chunks =
                                [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            }
                )
            ]
        , describe "padToLength"
            [ describe "FirstToLast"
                [ test "length less than current"
                    (\() ->
                        Array.Linear.padToLength 3 FirstToLast 0 num1234
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3 ])
                    )
                , test "length greater than current"
                    (\() ->
                        Array.Linear.padToLength 6 FirstToLast 0 num1234
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3, 4, 0, 0 ])
                    )
                , test "negative length"
                    (\() ->
                        Array.Linear.padToLength -1 FirstToLast 0 num1234
                            |> Expect.equal Array.empty
                    )
                ]
            , describe "LastToFirst"
                [ test "length less than current"
                    (\() ->
                        Array.Linear.padToLength 3 LastToFirst 0 num1234
                            |> Expect.equal
                                (Array.fromList [ 2, 3, 4 ])
                    )
                , test "length greater than current"
                    (\() ->
                        Array.Linear.padToLength 6 LastToFirst 0 num1234
                            |> Expect.equal
                                (Array.fromList [ 0, 0, 1, 2, 3, 4 ])
                    )
                , test "negative length"
                    (\() ->
                        Array.Linear.padToLength -1 FirstToLast 0 num1234
                            |> Expect.equal Array.empty
                    )
                ]
            ]
        ]



-- used


num1234 : Array number_
num1234 =
    Array.fromList [ 1, 2, 3, 4 ]
