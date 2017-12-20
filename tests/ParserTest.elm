module ParserTest exposing (..)


import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (..)

str = "string"

suite : Test
suite =
    describe "Parser"
        [ describe "Slash works for"
            [ test "path and int" <|
                \_ -> 
                    p str </> int
                        |> Expect.equal ( URLFork '/' (ParsePath str) (URLNode ParseInt) )
            , test "float and path" <|
                \_ -> 
                    float </> p str 
                        |> Expect.equal ( URLFork '/' ParseFloat (URLNode <| ParsePath str) )
            , test "float, int and path" <|
                \_ -> 
                    float </> int </> p str 
                        |> Expect.equal ( URLFork '/' ParseFloat <| URLFork '/' ParseInt (URLNode <| ParsePath str) )
            ]
        ]
