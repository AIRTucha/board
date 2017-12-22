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
        , describe "Parse path"
            [ test "just string" <|
                \_ ->
                    str   
                        |> parser (p str)
                        |> Expect.equal Succes
            , test "two strings" <|
                \_ ->
                    str ++ "/" ++ str
                        |> parser (p str </> p str)
                        |> Expect.equal Succes
            , test "sinle float" <|
                \_ ->
                    "3.1415"
                        |> parser float 
                        |> Expect.equal ( Floating 3.1415 ) 
            , test "single int" <|
                \_ ->
                    "10"
                        |> parser int 
                        |> Expect.equal ( Interger 10 ) 
            , test "string and int" <|
                \_ ->
                    str ++ "/10"
                        |> parser (p str </> int)
                        |> Expect.equal ( Interger 10 ) 
            , test "string and float" <|
                \_ ->
                    str ++ "/3.1415"
                        |> parser (p str </> float)
                        |> Expect.equal ( Floating 3.1415 ) 
            , test "two strings and float" <|
                \_ ->
                    str ++ "/3.1415/" ++ str
                        |> parser (p str </> float </> p str)
                        |> Expect.equal ( Floating 3.1415 ) 
            , test "float and int" <|
                \_ ->
                    "9" ++ "&3.1415"
                        |> parser ( int <&> float)
                        |> Expect.equal ( MultyValue <| (Interger 9) :: (Floating 3.1415 ) :: [] )
            , test "string, float and int" <|
                \_ ->
                    str ++ "/9" ++ "&3.1415"
                        |> parser ( p str </> int <&> float)
                        |> Expect.equal ( MultyValue <| (Interger 9) :: (Floating 3.1415 ) :: [] )
            , test "two strings, int and float" <|
                \_ ->  
                    str ++ "/3.1415" ++ "&9?" ++ str 
                        |> parser ( p str </> float <&> int <?> p str )
                        |> Expect.equal ( MultyValue <| (Floating 3.1415 ) :: (Interger 9) :: [] )
            , test "two strings, int and float mixed" <|
                \_ ->
                    let
                        str1 = str ++ "1"
                        str2 = str ++ "2"
                    in
                        str1 ++ "/3.1415?" ++ str2 ++ "&9"
                            |> parser ( p str1 </> float <?> p str2 <&> int )
                            |> Expect.equal ( MultyValue <| (Floating 3.1415 ) :: (Interger 9) :: [] )
            , test "three strings, int and float mixed" <|
                \_ ->
                    let
                        str1 = str ++ "1"
                        str2 = str ++ "2"
                        str3 = str ++ "3"
                    in
                        str1 ++ "/3.1415?" ++ str2 ++ "&9/" ++ str3
                            |> parser ( p str1 </> float <?> p str2 <&> int </> p str3 )
                            |> Expect.equal ( MultyValue <| (Floating 3.1415 ) :: (Interger 9) :: [] )
            ]
        ]
