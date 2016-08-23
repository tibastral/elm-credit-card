module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import CC exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "put Spaces every" <|
            \() ->
                Expect.true "Put Spaces is not working" (("0000" |> putSpacesEvery 4) == "0000")
        , test "put Spaces every" <|
            let
                res =
                    ("000000" |> putSpacesEvery 4)
            in
                \() ->
                    Expect.true ("it should show 0000 0 => " ++ res) (res == "0000 00")
        ]
