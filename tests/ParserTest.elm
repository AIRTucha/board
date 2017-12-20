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
            , test "complex path with single divider" <|
                \_ -> 
                    float </> int </> p str 
                        |> Expect.equal ( URLFork '/' ParseFloat <| URLFork '/' ParseInt (URLNode <| ParsePath str) )
            , test "complex path" <|
                \_ -> 
                    float </> p str <?> int
                        |> Expect.equal ( URLFork '/' ParseFloat <| URLFork '?' (ParsePath str) (URLNode <| ParseInt) )
            , test "verty comple path" <|
                \_ ->
                    (int </> int) <?> (float <&> p str)
                        |> Expect.equal 
                            ( URLFork '/' ParseInt <| 
                                URLFork '?' ParseInt <| 
                                    URLFork '&' ParseFloat <|
                                        URLNode (ParsePath str) )
            , test "path from two forks" <|
                \_ ->
                    (URLFork '/' ParseInt <| URLNode ParseInt ) <?> (URLFork '&' ParseFloat <| URLNode <| ParsePath str )
                        |> Expect.equal 
                            ( URLFork '/' ParseInt <| 
                                URLFork '?' ParseInt <| 
                                    URLFork '&' ParseFloat <|
                                        URLNode (ParsePath str) )
            ]
        ]
