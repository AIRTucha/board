module BoardTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Board exposing (..)
import Pathfinder exposing (..)

suite : Test
suite =
    describe "Board"
        [ test "true" <|
            \_ ->
                MultiValue [ Integer 10, Floating 3.1415, Str "ok"]
                    |> parsingResult2params
                    |> Expect.equal ( Ok <| MultiParam [ IntParam 10, FloatParam 3.1415, StrParam "ok"])
        ]