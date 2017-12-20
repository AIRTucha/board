module ParserTest exposing (..)


import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (..)
import Maybe

str = "string"

suite : Test
suite =
    describe "Parser"
        [ describe "Build a tree"
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
        , describe "Split string once"
            [ test "split string by /" <|
                \_ ->
                    break '/' "some/value"
                        |> Expect.equal ( Just ("some", "value") )
            , test "split string by multiple /" <|
                \_ ->
                    break '/' "some/value/someother"
                        |> Expect.equal ( Just ("some", "value/someother") )
            , test "empty string" <|
                \_ ->
                    break '/' ""
                        |> Expect.equal Nothing
            , test "no splitter" <|
                \_ ->
                    break '/' "some.value"
                        |> Expect.equal Nothing
            , test "just splitter" <|
                \_ ->
                    break '/' "/"
                        |> Expect.equal ( Just("","") )
            ]
        ]
