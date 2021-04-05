module Tests exposing (suite)

import Array
import Expect
import LinearDirection exposing (LinearDirection(..))
import LinearDirection.Array as Array
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "elm-linear-direction"
        [ describe "at"
            [ test "at FirstToLast returns the element at an index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Expect.all
                            [ Array.at 0 FirstToLast >> Expect.equal (Just 1)
                            , Array.at 1 FirstToLast >> Expect.equal (Just 2)
                            , Array.at 2 FirstToLast >> Expect.equal (Just 3)
                            , Array.at 3 FirstToLast >> Expect.equal (Just 4)
                            ]
                )
            , test "at LastToFirst returns the element at an index from the last"
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
            [ test "replaceAt FirstToLast sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.replaceAt 2 FirstToLast -3
                        |> Expect.equal (Array.fromList [ 1, 2, -3, 4 ])
                )
            , test "replaceAt LastToFirst sets the value at an index to a new value"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.replaceAt 1 LastToFirst -3
                        |> Expect.equal (Array.fromList [ 1, 2, -3, 4 ])
                )
            ]
        , describe "insertAt"
            [ test "insertAt FirstToLast puts the element after the elements before the index"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.insertAt 2 FirstToLast 123
                        |> Expect.equal (Array.fromList [ 1, 2, 123, 3, 4 ])
                )
            , test "insertAt LastToFirst puts the element after the elements before the index from last"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.insertAt 2 LastToFirst 123
                        |> Expect.equal (Array.fromList [ 1, 2, 123, 3, 4 ])
                )
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
        ]
