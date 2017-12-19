module ParserTest exposing (..)


import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (..)

str = "string"

suite : Test
suite =
    describe "Parser"
        [ describe "Slash"
            [ test "wosk for flat and param" <|
                \_ -> 
                    P str </> Integer
                        |> Expect.equal ( Collection <| (P str) :: Integer :: [] )
        ]

        ]
