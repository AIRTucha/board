module ParserTest exposing (..)


import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (..)
import Maybe

testStr = "string"

suite : Test
suite =
    describe "Parser"
        [ describe "Build a tree"
            [ test "path and int" <|
                \_ -> 
                    p testStr </> int
                        |> Expect.equal ( URLFork '/' (ParsePath testStr) (URLNode ParseInt) )
            , test "float and path" <|
                \_ -> 
                    float </> p testStr 
                        |> Expect.equal ( URLFork '/' ParseFloat (URLNode <| ParsePath testStr) )
            , test "complex path with single divider" <|
                \_ -> 
                    float </> int </> p testStr 
                        |> Expect.equal ( URLFork '/' ParseFloat <| URLFork '/' ParseInt (URLNode <| ParsePath testStr) )
            , test "complex path" <|
                \_ -> 
                    float </> p testStr <?> int
                        |> Expect.equal ( URLFork '/' ParseFloat <| URLFork '?' (ParsePath testStr) (URLNode <| ParseInt) )
            , test "verty comple path" <|
                \_ ->
                    (int </> int) <?> (float <&> p testStr)
                        |> Expect.equal 
                            ( URLFork '/' ParseInt <| 
                                URLFork '?' ParseInt <| 
                                    URLFork '&' ParseFloat <|
                                        URLNode (ParsePath testStr) )
            , test "path from two forks" <|
                \_ ->
                    (URLFork '/' ParseInt <| URLNode ParseInt ) <?> (URLFork '&' ParseFloat <| URLNode <| ParsePath testStr )
                        |> Expect.equal 
                            ( URLFork '/' ParseInt <| 
                                URLFork '?' ParseInt <| 
                                    URLFork '&' ParseFloat <|
                                        URLNode (ParsePath testStr) )
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
            [ describe "Path"
                [ describe "Correct"
                    [ test "just path" <|
                        \_ ->
                            testStr   
                                |> parser (p testStr)
                                |> Expect.equal Succes
                    , test "two paths" <|
                        \_ ->
                            testStr ++ "/" ++ testStr
                                |> parser (p testStr </> p testStr)
                                |> Expect.equal Succes
                    , test "path and int" <|
                        \_ ->
                            testStr ++ "/10"
                                |> parser (p testStr </> int)
                                |> Expect.equal ( Interger 10 ) 
                    , test "path and float" <|
                        \_ ->
                            testStr ++ "/3.1415"
                                |> parser (p testStr </> float)
                                |> Expect.equal ( Floating 3.1415 )
                    ]
                , describe "Error" 
                    [ test "Incorrect path" <|
                        \_ ->
                            let
                                strErr = testStr ++ "Error"
                            in
                                strErr
                                    |> parser (p testStr)
                                    |> Expect.equal ( Failure <| testStr ++ " is not " ++ strErr )
                    , test "Incorrect path after divider" <|
                        \_ ->
                            let
                                str1   = testStr ++ "1"
                                strErr = testStr ++ "Error"
                            in
                                str1 ++ "/" ++ strErr
                                    |> parser ( p str1 </> p testStr )
                                    |> Expect.equal ( Failure <| testStr ++ " is not " ++ strErr )
                    , test "Incorrect devider between paths" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                            in
                                str1 ++ "/" ++ str2
                                    |> parser (p str1 <&> p str2)
                                    |> Expect.equal (Failure <| str1 ++ "& is not " ++ str1 ++ "/")     
                    ]
                ]
                , describe "Integer"
                    [ describe "Correct"
                        [ test "single int" <|
                            \_ ->
                                "10"
                                    |> parser int 
                                    |> Expect.equal ( Interger 10 ) 
                        , test "two ints" <|
                            \_ ->
                                "10/9"
                                    |> parser  (int </> int) 
                                    |> Expect.equal ( MultyValue <| Interger 10 :: Interger 9 :: [] )
                        , test "int and path" <|
                            \_ ->
                                "9/" ++ testStr
                                    |> parser (int </> p testStr)
                                    |> Expect.equal ( Interger 9 )
                        , test "int and float" <|
                            \_ ->
                                "10/9.123"
                                    |> parser  (int </> float) 
                                    |> Expect.equal ( MultyValue <| Interger 10 :: Floating 9.123 :: [] )
                        ]
                    , describe "Error"
                        [ test "Incorrect int" <|
                            \_ ->
                                "a9"
                                    |> parser int
                                    |> Expect.equal (Failure "could not convert string 'a9' to an Int")
                        , test "Incorrect separator between ints" <|
                            \_ ->
                                "10?43"
                                    |> parser (int </> int)
                                    |> Expect.equal ( Failure <| "10?43 does not contain /")
                        ]
                    ]
            ]
        ]




            --         , describe "Correct"
            --     [ test "sinle float" <|
            --         \_ ->
            --             "3.1415"
            --                 |> parser float 
            --                 |> Expect.equal ( Floating 3.1415 ) 
         
            --     , test "two strings and float" <|
            --         \_ ->
            --             testStr ++ "/3.1415/" ++ str
            --                 |> parser (p testStr </> float </> p str)
            --                 |> Expect.equal ( Floating 3.1415 ) 
            --     , test "float and int" <|
            --         \_ ->
            --             "9" ++ "&3.1415"
            --                 |> parser ( int <&> float)
            --                 |> Expect.equal ( MultyValue <| (Interger 9) :: (Floating 3.1415 ) :: [] )
            --     , test "string, float and int" <|
            --         \_ ->
            --             testStr ++ "/9" ++ "&3.1415"
            --                 |> parser ( p testStr </> int <&> float)
            --                 |> Expect.equal ( MultyValue <| (Interger 9) :: (Floating 3.1415 ) :: [] )
            --     , test "two strings, int and float" <|
            --         \_ ->  
            --             testStr ++ "/3.1415" ++ "&9?" ++ testStr 
            --                 |> parser ( p testStr </> float <&> int <?> p testStr )
            --                 |> Expect.equal ( MultyValue <| (Floating 3.1415 ) :: (Interger 9) :: [] )
            --     , test "two strings, int and float mixed" <|
            --         \_ ->
            --             let
            --                 str1 = testStr ++ "1"
            --                 str2 = testStr ++ "2"
            --             in
            --                 str1 ++ "/3.1415?" ++ str2 ++ "&9"
            --                     |> parser ( p str1 </> float <?> p str2 <&> int )
            --                     |> Expect.equal ( MultyValue <| (Floating 3.1415 ) :: (Interger 9) :: [] )
            --     , test "three strings, int and float mixed" <|
            --         \_ ->
            --             let
            --                 str1 = testStr ++ "1"
            --                 str2 = testStr ++ "2"
            --                 str3 = testStr ++ "3"
            --             in
            --                 str1 ++ "/3.1415?" ++ str2 ++ "&9/" ++ str3
            --                     |> parser ( p str1 </> float <?> p str2 <&> int </> p str3 )
            --                     |> Expect.equal ( MultyValue <| (Floating 3.1415 ) :: (Interger 9) :: [] )
            --     ]
            -- , describe "Errors"

            --     , test "Incorrect float" <|
            --             \_ ->
            --                 "a9"
            --                     |> parser float
            --                     |> Expect.equal (Failure "could not convert string 'a9' to a Float")
            --     , test "Absents of separator bertween strings" <|
            --         \_ ->
            --             testStr ++ testStr 
            --                 |> parser ( p testStr </> p testStr )
            --                 |> Expect.equal (Failure <| testStr ++ "/ is not " ++ testStr ++ String.left 1 str)
            --     , test "Absents of separator bertween int and string" <|
            --         \_ ->
            --             "9" ++ str
            --                 |> parser ( int </> p testStr )
            --                 |> Expect.equal (Failure <| "9" ++ testStr ++ " does not contain /")
            --     , test "Error in multiple values" <|
            --         \_ ->
            --             testStr ++ "/10/g14.2"
            --                 |> parser ( p testStr </> int </> float )
            --                 |> Expect.equal ( MultyValue <| Interger 10 :: Failure "could not convert string 'g14.2' to a Float" :: [] )
            --     ]
