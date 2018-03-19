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
            -- ( \ (MultiValue [ Integer i, Floating f] )-> i)
                a (int </> float) "10/20" ( \ (MultiValue [Integer i, Floating f] )-> Finish )
                    |> Expect.equal Finish
        ]