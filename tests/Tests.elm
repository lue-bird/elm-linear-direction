module Tests exposing (suite)

import Array exposing (Array)
import Array.LinearDirection as Array
import Expect
import LinearDirection exposing (LinearDirection(..))
import List.LinearDirection as List
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
        [ describe "group"
            [ test "FirstToLast"
                (\() ->
                    [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> List.group 3 FirstToLast
                        |> Expect.equal
                            { groups = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
                            , less = [ 7 ]
                            }
                )
            , test "LastToFirst"
                (\() ->
                    [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> List.group 3 LastToFirst
                        |> Expect.equal
                            { less = [ 1 ]
                            , groups = [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
                            }
                )
            ]
        , describe "takeFrom"
            [ test "FirstToLast"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.takeFrom FirstToLast 2
                        |> Expect.equal [ 1, 2 ]
                )
            , test "LastToFirst"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.takeFrom LastToFirst 2
                        |> Expect.equal [ 4, 5 ]
                )
            ]
        , describe "dropFrom"
            [ test "FirstToLast"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.dropFrom FirstToLast 2
                        |> Expect.equal [ 3, 4, 5 ]
                )
            , test "LastToFirst"
                (\() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> List.dropFrom LastToFirst 2
                        |> Expect.equal [ 1, 2, 3 ]
                )
            ]
        , describe "at"
            [ test "FirstToLast at valid index"
                (\() ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.at 0 LastToFirst
                        |> Expect.equal (Just 4)
                )
            , test "LastToFirst at valid index"
                (\() ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.at 2 FirstToLast
                        |> Expect.equal (Just 2)
                )
            , test "FirstToLast index >= length"
                (\() ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.at 5 FirstToLast
                        |> Expect.equal Nothing
                )
            , test "FirstToLast at negative index"
                (\() ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.at -1 FirstToLast
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
                            [ Array.at 0 FirstToLast >> Expect.equal (Just 1)
                            , Array.at 1 FirstToLast >> Expect.equal (Just 2)
                            , Array.at 2 FirstToLast >> Expect.equal (Just 3)
                            , Array.at 3 FirstToLast >> Expect.equal (Just 4)
                            ]
                )
            , test "FirstToLast at a too high index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.at 100 FirstToLast
                        |> Expect.equal Nothing
                )
            , test "LastToFirst at a too high index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.at 100 LastToFirst
                        |> Expect.equal Nothing
                )
            , test "LastToFirst at a valid index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Expect.all
                            [ Array.at 0 LastToFirst >> Expect.equal (Just 4)
                            , Array.at 1 LastToFirst >> Expect.equal (Just 3)
                            , Array.at 2 LastToFirst >> Expect.equal (Just 2)
                            , Array.at 3 LastToFirst >> Expect.equal (Just 1)
                            ]
                )
            ]
        , describe "replaceAt"
            [ test "valid index FirstToLast sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.replaceAt 2 FirstToLast -3
                        |> Expect.equal (Array.fromList [ 1, 2, -3, 4 ])
                )
            , test "negative index FirstToLast changes nothing"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.replaceAt -1 FirstToLast 123
                        |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
                )
            , test "too high index FirstToLast changes nothing"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.replaceAt 100 FirstToLast 123
                        |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
                )
            , test "valid index LastToFirst sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.replaceAt 1 LastToFirst -3
                        |> Expect.equal (Array.fromList [ 1, 2, -3, 4 ])
                )
            , test "negative index LastToFirst changes nothing"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.replaceAt -1 LastToFirst 123
                        |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
                )
            , test "too high index LastToFirst changes nothing"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.replaceAt 100 LastToFirst 123
                        |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
                )
            ]
        , describe "insertAt"
            [ describe "FirstToLast"
                [ test "valid index"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.insertAt 2 FirstToLast 123
                            |> Expect.equal (Array.fromList [ 1, 2, 123, 3, 4 ])
                    )
                , test "length"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.insertAt 4 FirstToLast 123
                            |> Expect.equal (Array.fromList [ 1, 2, 3, 4, 123 ])
                    )
                , test "negative index"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.insertAt -1 FirstToLast 123
                            |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
                    )
                , test "too high index"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.insertAt 100 FirstToLast 123
                            |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
                    )
                ]
            , describe "LastToFirst"
                [ test "valid index"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.insertAt 2 LastToFirst 123
                            |> Expect.equal (Array.fromList [ 1, 2, 123, 3, 4 ])
                    )
                , test "length"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.insertAt 4 LastToFirst 123
                            |> Expect.equal (Array.fromList [ 123, 1, 2, 3, 4 ])
                    )
                , test "negative index"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.insertAt -1 LastToFirst 123
                            |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
                    )
                , test "too high index"
                    (\() ->
                        Array.fromList [ 1, 2, 3, 4 ]
                            |> Array.insertAt 100 LastToFirst 123
                            |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
                    )
                ]
            ]
        , describe "removeAt"
            [ test "removeAt FirstToLast removes the element at the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.removeAt 2 FirstToLast
                        |> Expect.equal (Array.fromList [ 1, 2, 4 ])
                )
            , test "removeAt LastToFirst removes the element at the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.removeAt 1 LastToFirst
                        |> Expect.equal (Array.fromList [ 1, 2, 4 ])
                )
            ]
        , describe "take"
            [ test "take FirstToLast"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.take 3 FirstToLast
                        |> Expect.equal (Array.fromList [ 1, 2, 3 ])
                )
            , test "take LastToFirst"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.take 3 LastToFirst
                        |> Expect.equal (Array.fromList [ 2, 3, 4 ])
                )
            ]
        , describe "drop"
            [ test "drop FirstToLast"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.drop 1 FirstToLast
                        |> Expect.equal (Array.fromList [ 2, 3, 4 ])
                )
            , test "drop LastToFirst"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.drop 1 LastToFirst
                        |> Expect.equal (Array.fromList [ 1, 2, 3 ])
                )
            ]
        , describe "group"
            [ test "group FirstToLast"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> Array.group 3 FirstToLast
                        |> Expect.equal
                            { groups =
                                [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            , less = Array.fromList [ 7 ]
                            }
                )
            , test "group LastToFirst"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
                        |> Array.group 3 LastToFirst
                        |> Expect.equal
                            { less = Array.fromList [ 1 ]
                            , groups =
                                [ [ 2, 3, 4 ], [ 5, 6, 7 ] ]
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            }
                )
            ]
        , describe "resize"
            [ describe "FirstToLast"
                [ test "length less than current"
                    (\() ->
                        Array.resize FirstToLast 3 0 num1234
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3 ])
                    )
                , test "length greater than current"
                    (\() ->
                        Array.resize FirstToLast 6 0 num1234
                            |> Expect.equal
                                (Array.fromList [ 1, 2, 3, 4, 0, 0 ])
                    )
                , test "negative length"
                    (\() ->
                        Array.resize FirstToLast -1 0 num1234
                            |> Expect.equal Array.empty
                    )
                ]
            , describe "LastToFirst"
                [ test "length less than current"
                    (\() ->
                        Array.resize LastToFirst 3 0 num1234
                            |> Expect.equal
                                (Array.fromList [ 2, 3, 4 ])
                    )
                , test "length greater than current"
                    (\() ->
                        Array.resize LastToFirst 6 0 num1234
                            |> Expect.equal
                                (Array.fromList [ 0, 0, 1, 2, 3, 4 ])
                    )
                , test "negative length"
                    (\() ->
                        Array.resize FirstToLast -1 0 num1234
                            |> Expect.equal Array.empty
                    )
                ]
            ]
        ]



-- used


num1234 : Array number_
num1234 =
    Array.fromList [ 1, 2, 3, 4 ]
