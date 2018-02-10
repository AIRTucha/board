module ParserTest exposing (..)


import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (..)
import Dict exposing(..)
import Maybe
import Debug 

testStr = "string"

suite : Test
suite =
    describe "Parser"
        [ describe "Split string once"
            [ test "split string by /" <|
                \_ ->
                    break '/' "some/value"
                        |> Expect.equal ( Ok ("some", "value") )
            , test "split string by multiple /" <|
                \_ ->
                    break '/' "some/value/someother"
                        |> Expect.equal ( Ok ("some", "value/someother") )
            , test "empty string" <|
                \_ ->
                    break '/' ""
                        |> Expect.equal (Err " does not contain /")
            , test "no splitter" <|
                \_ ->
                    break '/' "some.value"
                        |> Expect.equal (Err "some.value does not contain /")
            , test "just splitter" <|
                \_ ->
                    break '/' "/"
                        |> Expect.equal ( Ok ("","") )
            ]
        , describe "Build a tree"
            [ describe "Ordered devider between" 
                [ test "two simple nodes" <|
                    \_ -> 
                        p testStr </> int
                            |> Expect.equal ( OrderedURL '/' (NodeURL <| ParsePath testStr) (NodeURL ParseInt) )
                , test "three simple nodes" <|
                    \_ -> 
                        float </> int <?> p testStr 
                            |> Expect.equal ( OrderedURL '/' (NodeURL ParseFloat) <| OrderedURL '?' (NodeURL ParseInt) (NodeURL <| ParsePath testStr) )
                , test "three simple nodes, left" <|
                    \_ -> 
                        float <?> (int </> p testStr)
                            |> Expect.equal ( OrderedURL '?' (NodeURL ParseFloat) <| OrderedURL '/' (NodeURL ParseInt) (NodeURL <| ParsePath testStr) )
                , test "three simple nodes, right" <|
                    \_ -> 
                        (float </> int) <&> p testStr 
                            |> Expect.equal ( UnorderedURL '&' [ OrderedURL '/' (NodeURL ParseFloat) (NodeURL ParseInt), NodeURL <| ParsePath testStr ])
                , test "node and two other nodes devided unorderedly" <|
                    \_ -> 
                        float </> int <&> p testStr 
                            |> Expect.equal ( OrderedURL '/' (NodeURL ParseFloat) ( UnorderedURL '&' [NodeURL ParseInt,NodeURL (ParsePath testStr)] ) )
                , test "node and two other nodes devided unorderedly, left" <|
                    \_ -> 
                        (float </> int) <&> p testStr 
                            |> Expect.equal ( UnorderedURL '&' [ OrderedURL '/' (NodeURL ParseFloat) (NodeURL ParseInt), NodeURL <| ParsePath testStr ])
                , test "node and two other nodes devided unorderedly, right" <|
                    \_ -> 
                        float </> (int <&> p testStr)
                            |> Expect.equal ( OrderedURL '/' (NodeURL ParseFloat) ( UnorderedURL '&' [NodeURL ParseInt,NodeURL (ParsePath testStr)] ) )
                , test "two ordered subtrees" <|
                    \_ ->
                        (int </> float) <?> (str </> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (OrderedURL '/' (NodeURL ParseInt) (NodeURL ParseFloat)) 
                                    (OrderedURL '/' (NodeURL ParseStr) (NodeURL (ParsePath testStr)))
                                )
                , test "ordered and unordered subtrees" <|
                    \_ ->
                        (int </> float) <?> (str <&> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (OrderedURL '/' (NodeURL ParseInt) (NodeURL ParseFloat))
                                    (UnorderedURL '&' [NodeURL ParseStr, NodeURL (ParsePath testStr)])
                                )
                , test "unordered and ordered subtrees" <|
                    \_ ->
                        (int <*> float) <?> (str </> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (UnorderedURL '*' [NodeURL ParseInt, NodeURL ParseFloat]) 
                                    (OrderedURL '/' (NodeURL ParseStr) (NodeURL (ParsePath testStr)))
                                )
                , test "two unordered subtrees" <|
                    \_ ->
                        (int <&> float) <?> (str <&> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (UnorderedURL '&' [NodeURL ParseInt, NodeURL ParseFloat]) 
                                    (UnorderedURL '&' [NodeURL ParseStr, NodeURL (ParsePath testStr)])
                                )
                ]
            , describe "Unordered devider between"
                [ test "two simple nodes" <|
                    \_ -> 
                        float <&> p testStr 
                            |> Expect.equal ( UnorderedURL '&' [NodeURL ParseFloat, NodeURL (ParsePath testStr)])
                , test "three simple nodes" <|
                    \_ -> 
                        float <&> int <&> p testStr
                            |> Expect.equal ( UnorderedURL '&' [ NodeURL ParseFloat, NodeURL ParseInt, NodeURL <| ParsePath testStr] )
                , test "three simple nodes, left" <|
                    \_ -> 
                        (float <&> int) <&> p testStr
                            |> Expect.equal ( UnorderedURL '&' [ NodeURL ParseFloat, NodeURL ParseInt, NodeURL <| ParsePath testStr] )
                , test "three simple nodes, right" <|
                    \_ -> 
                        float <&> (int <&> p testStr)
                            |> Expect.equal ( UnorderedURL '&' [ NodeURL ParseFloat, NodeURL ParseInt, NodeURL <| ParsePath testStr] )
                , test "node and two other nodes devided orderedly" <|
                    \_ -> 
                        float <&> int <?> p testStr
                            |> Expect.equal ( OrderedURL '?' (UnorderedURL '&' ([NodeURL ParseFloat,NodeURL ParseInt])) (NodeURL (ParsePath testStr)) )
                , test "node and two other nodes devided orderedly, left" <|
                    \_ -> 
                        (float <&> int) <?> p testStr
                            |> Expect.equal ( OrderedURL '?' (UnorderedURL '&' ([NodeURL ParseFloat,NodeURL ParseInt])) (NodeURL (ParsePath testStr)) )
                , test "node and two other nodes devided orderedly, right" <|
                    \_ -> 
                        float <&> (int <?> p testStr)
                            |> Expect.equal (  UnorderedURL '&' ([NodeURL ParseFloat,OrderedURL '?' (NodeURL ParseInt) (NodeURL (ParsePath testStr))]) )
                , test "node and two other nodes devided unorderedly with different char" <|
                    \_ ->
                        int <&> str <*> any
                            |> Expect.equal (UnorderedURL '*' [UnorderedURL '&' [NodeURL ParseInt, NodeURL ParseStr], NodeURL ParseAny])
                , test "node and two other nodes devided unorderedly with different char, left" <|
                    \_ ->
                        (int <&> str) <*> any
                            |> Expect.equal (UnorderedURL '*' [UnorderedURL '&' [NodeURL ParseInt, NodeURL ParseStr], NodeURL ParseAny])
                , test "node and two other nodes devided unorderedly with different char, right" <|
                    \_ ->
                        int <&> (str <*> any)
                            |> Expect.equal (UnorderedURL '&' [NodeURL ParseInt, UnorderedURL '*' [NodeURL ParseStr, NodeURL ParseAny]])
                , test "two ordered subtrees" <|
                    \_ ->
                        (int </> float) <&> (str </> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&'
                                    [ (OrderedURL '/' (NodeURL ParseInt) (NodeURL ParseFloat)) 
                                    , (OrderedURL '/' (NodeURL ParseStr) (NodeURL (ParsePath testStr)))
                                    ]
                                )
                , test "ordered and unordered subtrees" <|
                    \_ ->
                        (int </> float) <?> (str <&> p testStr)
                            |> Expect.equal 
                                ( OrderedURL '?' 
                                    (OrderedURL '/' (NodeURL ParseInt) (NodeURL ParseFloat))
                                    (UnorderedURL '&' [NodeURL ParseStr, NodeURL (ParsePath testStr)])
                                )
                , test "unordered and ordered subtrees" <|
                    \_ ->
                        (int <*> float) <&> (str </> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&' 
                                    [ (UnorderedURL '*' [NodeURL ParseInt, NodeURL ParseFloat]) 
                                    , (OrderedURL '/' (NodeURL ParseStr) (NodeURL (ParsePath testStr)))
                                    ]
                                )
                , test "two unordered subtrees" <|
                    \_ ->
                        (int <&> float) <&> (str <&> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&' ([NodeURL ParseInt,NodeURL ParseFloat,NodeURL ParseStr,NodeURL (ParsePath testStr)]))
                , test "two unordered subtrees, with different char" <|
                    \_ ->
                        (int <*> float) <&> (str <*> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&' 
                                    [(UnorderedURL '*' [NodeURL ParseInt, NodeURL ParseFloat]) 
                                    ,(UnorderedURL '*' [NodeURL ParseStr, NodeURL (ParsePath testStr)])
                                    ]
                                )
                , test "two unordered subtrees, with different chars between nodes, left" <|
                    \_ ->
                        (int <&> float) <&> (str <*> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '&' 
                                    [ NodeURL ParseInt
                                    , NodeURL ParseFloat
                                    , UnorderedURL '*' [NodeURL ParseStr, NodeURL (ParsePath testStr)]
                                    ]
                                )
                , test "two unordered subtrees, with different chars between nodes, right" <|
                    \_ ->
                        (int <&> float) <*> (str <*> p testStr)
                            |> Expect.equal 
                                ( UnorderedURL '*' 
                                    ([ UnorderedURL '&' ([NodeURL ParseInt, NodeURL ParseFloat])
                                    ,  NodeURL ParseStr
                                    ,  NodeURL (ParsePath testStr)
                                    ])
                                )
                ]
            ]
        , describe "Parse path"
            [ describe "Path"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            testStr   
                                |> parser (p testStr)
                                |> Expect.equal Succes
                    , describe "Ordered" <|
                        [ test "two paths" <|
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
                        , test "path and string" <|
                            \_ ->
                                testStr ++ "/" ++ testStr
                                    |> parser (p testStr </> str)
                                    |> Expect.equal ( Str testStr )
                        , test "path and any" <|
                            \_ ->
                                testStr ++ "/" ++ testStr
                                    |> parser (p testStr </> any)
                                    |> Expect.equal ( Succes )
                        , test "path and query" <|
                            \_ ->
                                testStr ++ "/" ++ testStr ++ "=" ++ testStr
                                    |> parser (p testStr </> query)
                                    |> Expect.equal ( Query <| Dict.fromList [(testStr, testStr)])
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "two paths" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr1 ++ "*" ++ testStr2
                                            |> parser (p testStr1 <*> p testStr2)
                                            |> Expect.equal Succes
                            , test "path or int" <|
                                \_ ->
                                    testStr ++ "&10"
                                        |> parser (p testStr <&> int)
                                        |> Expect.equal ( Interger 10 ) 
                            , test "path or float" <|
                                \_ ->
                                    testStr ++ "&3.1415"
                                        |> parser (p testStr <&> float)
                                        |> Expect.equal ( Floating 3.1415 )
                            , test "path or string" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr1 ++ "*" ++ testStr2
                                            |> parser (p testStr1 <*> str)
                                            |> Expect.equal ( Str testStr2 )
                            , test "path or any" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr1 ++ "&" ++ testStr2
                                            |> parser (p testStr1 <&> any)
                                            |> Expect.equal ( Succes )
                            , test "path or query" <|
                                \_ ->
                                    testStr ++ "&" ++ testStr ++ "=" ++ testStr
                                        |> parser (p testStr <&> query)
                                        |> Expect.equal ( Query <| Dict.fromList [(testStr, testStr)])
                            ]
                        , describe "inverted"
                            [ test "two paths" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr2 ++ "*" ++ testStr1
                                            |> parser (p testStr1 <*> p testStr2)
                                            |> Expect.equal Succes
                            , test "path or int" <|
                                \_ ->
                                    "10&" ++ testStr
                                        |> parser (p testStr <&> int)
                                        |> Expect.equal ( Interger 10 ) 
                            , test "path or float" <|
                                \_ ->
                                    "3.1415&" ++ testStr
                                        |> parser (p testStr <&> float)
                                        |> Expect.equal ( Floating 3.1415 )
                            , test "path or string" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr2 ++ "*" ++ testStr1
                                            |> parser (p testStr1 <*> str)
                                            |> Expect.equal ( Str testStr2 )
                            , test "path or any" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr2 ++ "&" ++ testStr1
                                            |> parser (p testStr1 <&> any)
                                            |> Expect.equal ( Succes )
                            , test "path or query" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr1 ++ "&" ++ testStr2 ++ "=" ++ testStr
                                            |> parser (p testStr1 <&> query)
                                            |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr)])
                            ]
                        ]
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
                    , test "Incorrect path after ordered divider" <|
                        \_ ->
                            let
                                str1   = testStr ++ "1"
                                strErr = testStr ++ "Error"
                            in
                                str1 ++ "/" ++ strErr
                                    |> parser ( p str1 </> p testStr )
                                    |> Expect.equal ( Failure <| testStr ++ " is not " ++ strErr )
                    , test "Incorrect path after unordered divider" <|
                        \_ ->
                            let
                                str1   = testStr ++ "1"
                                strErr = testStr ++ "Error"
                            in
                                str1 ++ "&" ++ strErr
                                    |> parser ( p str1 <&> p testStr )
                                    |> Expect.equal ( Failure <| "Start of " ++ strErr ++ " does not have any value which can be corectly parsed by: Path string, separated by &." )
                    , test "Incorrect ordered devider between paths" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                                path = str1 ++ "/" ++ str2
                            in
                                path
                                    |> parser (p str1 <?> p str2)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between paths" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                                path = str1 ++ "&" ++ str2
                            in
                                path
                                    |> parser (p str1 <*> p str2)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Path " ++ str1 ++ " or Path " ++ str2 ++ ", separated by *." )     
                    , test "Incorrect ordered devider instead of unordered one between paths" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                                path = str1 ++ "/" ++ str2
                            in
                                path
                                    |> parser (p str1 <?> p str2)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )     
                    , test "Incorrect unordered devider instead of ordered one between paths" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                                path = str1 ++ "/" ++ str2
                            in
                                path
                                    |> parser (p str1 <*> p str2)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Path " ++ str1 ++ " or Path " ++ str2 ++ ", separated by *." )      
                    ]
                ]
            , describe "Integer"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            "10"
                                |> parser int 
                                |> Expect.equal ( Interger 10 ) 
                    , describe "Ordered" <|
                        [ test "two ints" <|
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
                        , test "int and string" <|
                            \_ ->
                                "10/" ++ testStr
                                    |> parser  (int </> str) 
                                    |> Expect.equal ( MultyValue <| Interger 10 :: Str testStr :: [] )
                        , test "int and any" <|
                            \_ ->
                                "10/" ++ testStr
                                    |> parser  (int </> any) 
                                    |> Expect.equal ( Interger 10 )
                        , test "int and query" <|
                            \_ ->
                                "10/" ++ testStr ++ "=" ++ testStr
                                    |> parser (int </> query)
                                    |> Expect.equal ( MultyValue 
                                        [ Interger 10
                                        , Query <| Dict.fromList [(testStr, testStr)]
                                        ]
                                    )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "int or path" <|
                                \_ ->
                                     "10&" ++ testStr
                                        |> parser (int <&> p testStr)
                                        |> Expect.equal ( Interger 10 ) 
                            , test "int or float" <|
                                \_ ->
                                    "10&3.1415"
                                        |> parser (int <&> float )
                                        |> Expect.equal ( MultyValue <| [Interger 10, Floating 3.1415] )
                            , test "int or string" <|
                                \_ ->
                                    "10*" ++ testStr
                                        |> parser (int <*> str)
                                        |> Expect.equal ( MultyValue <| [Interger 10, Str testStr] )
                            , test "int or any" <|
                                \_ ->
                                        "10&" ++ testStr
                                            |> parser ( int <&> any)
                                            |> Expect.equal ( Interger 10 )
                            , test "int or query" <|
                                \_ ->
                                    "10&" ++ testStr ++ "=" ++ testStr
                                        |> parser ( int <&> query )
                                        |> Expect.equal (MultyValue <| [ Interger 10, Query <| Dict.fromList [(testStr, testStr)] ])
                            ]
                        , describe "inverted"
                            [ test "int or path" <|
                                \_ ->
                                     testStr ++ "&10"
                                        |> parser (int <&> p testStr)
                                        |> Expect.equal ( Interger 10 ) 
                            , test "int or float" <|
                                \_ ->
                                    "3.1415&10"
                                        |> parser (int <&> float )
                                        |> Expect.equal ( MultyValue <| [Interger 10, Floating 3.1415] )
                            , test "int or string" <|
                                \_ ->
                                    testStr ++ "*10"
                                        |> parser (int <*> str)
                                        |> Expect.equal ( MultyValue <| [Interger 10, Str testStr] )
                            , test "int or any" <|
                                \_ ->
                                    testStr ++ "&10"
                                        |> parser ( int <&> any)
                                        |> Expect.equal ( Interger 10 )
                            , test "int or query" <|
                                \_ ->
                                    testStr ++ "=" ++ testStr ++ "&10"
                                        |> parser ( int <&> query )
                                        |> Expect.equal (MultyValue <| [ Interger 10, Query <| Dict.fromList [(testStr, testStr)] ])
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect int" <|
                        \_ ->
                            "9.14"
                                |> parser int
                                |> Expect.equal (Failure "could not convert string '9.14' to an Int")
                    , test "Incorrect separator between ints" <|
                        \_ ->
                            "10?43"
                                |> parser (int </> int)
                                |> Expect.equal ( Failure <| "10?43 does not contain /")
                    , test "Incorrect int after ordered devider" <|
                        \_ ->
                            "5?a3"
                                |> parser (int <?> int)
                                |> Expect.equal ( Failure "could not convert string 'a3' to an Int" )
                    , test "Incorrect int after unordered divider" <|
                        \_ ->
                            "10&a43"
                                |> parser ( int <&> int )
                                |> Expect.equal ( Failure <| "Start of a43 does not have any value which can be corectly parsed by: Int, separated by &." )
                    , test "Incorrect ordered devider between paths" <|
                        \_ ->
                            let
                                path = "10/34"
                            in
                                path
                                    |> parser (int <?> int)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between paths" <|
                        \_ ->
                            let
                                path = "10&23"
                            in
                                path
                                    |> parser (int <*> int)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Int or Int, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between paths" <|
                        \_ ->
                            let
                                path = "10/2"
                            in
                                path
                                    |> parser (int <&> int)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Int or Int, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between paths" <|
                        \_ ->
                            let
                                path = "10*20"
                            in
                                path
                                    |> parser (int </> int)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            , describe "Floating"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            "3.1415"
                                |> parser float 
                                |> Expect.equal ( Floating 3.1415 ) 
                    , describe "Ordered" <|
                        [ test "two floats" <|
                            \_ ->
                                "3.1415/43.2"
                                    |> parser  (float </> float) 
                                    |> Expect.equal ( MultyValue <| Floating 3.1415 :: Floating 43.2 :: [] )
                        , test "float and path" <|
                            \_ ->
                                "3.1415/" ++ testStr
                                    |> parser (float </> p testStr)
                                    |> Expect.equal ( Floating 3.1415 )
                        , test "float and float" <|
                            \_ ->
                                "3.1415/9"
                                    |> parser  (float </> int) 
                                    |> Expect.equal ( MultyValue <| Floating 3.1415 :: Interger 9 :: [] )
                        , test "float and string" <|
                            \_ ->
                                "3.1415/" ++ testStr
                                    |> parser  (float </> str) 
                                    |> Expect.equal ( MultyValue <| Floating 3.1415 :: Str testStr :: [] )
                        , test "float and any" <|
                            \_ ->
                                "3.1415/" ++ testStr
                                    |> parser  (float </> any) 
                                    |> Expect.equal ( Floating 3.1415 )
                        , test "float and query" <|
                            \_ ->
                                "3.1415/" ++ testStr ++ "=" ++ testStr
                                    |> parser (float </> query)
                                    |> Expect.equal ( MultyValue 
                                        [ Floating 3.1415
                                        , Query <| Dict.fromList [(testStr, testStr)]
                                        ]
                                    )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "float or path" <|
                                \_ ->
                                     "3.1415&" ++ testStr
                                        |> parser (float <&> p testStr)
                                        |> Expect.equal ( Floating 3.1415 ) 
                            , test "float or int" <|
                                \_ ->
                                    "3.1415&3"
                                        |> parser (float <&> int )
                                        |> Expect.equal ( MultyValue <| [Floating 3.1415, Interger 3] )
                            , test "float or string" <|
                                \_ ->
                                    "3.1415*" ++ testStr
                                        |> parser (float <*> str)
                                        |> Expect.equal ( MultyValue <| [Floating 3.1415, Str testStr] )
                            , test "float or any" <|
                                \_ ->
                                        "3.1415&" ++ testStr
                                            |> parser ( float <&> any)
                                            |> Expect.equal ( Floating 3.1415 )
                            , test "float or query" <|
                                \_ ->
                                    "3.1415&" ++ testStr ++ "=" ++ testStr
                                        |> parser ( float <&> query )
                                        |> Expect.equal (MultyValue <| [ Floating 3.1415, Query <| Dict.fromList [(testStr, testStr)] ])
                            ]
                        , describe "inverted"
                            [ describe "limitations" <|
                                    [ test "float or int" <|
                                        \_ ->
                                            "3&3.1415"
                                                |> parser (float <&> int )
                                                |> Expect.equal ( Failure "Start of 3.1415 does not have any value which can be corectly parsed by: Int, separated by &." )

                                    ]
                            , test "float or path" <|
                                \_ ->
                                     testStr ++ "&3.1415"
                                        |> parser (float <&> p testStr)
                                        |> Expect.equal ( Floating 3.1415 )
                            , test "float or string" <|
                                \_ ->
                                    testStr ++ "*3.1415"
                                        |> parser (float <*> str)
                                        |> Expect.equal ( MultyValue <| [Floating 3.1415, Str testStr] )
                            , test "float or any" <|
                                \_ ->
                                    testStr ++ "&10"
                                        |> parser (float <&> any)
                                        |> Expect.equal ( Floating 10 )
                            , test "float or query" <|
                                \_ ->
                                    testStr ++ "=" ++ testStr ++ "&10"
                                        |> parser ( float <&> query )
                                        |> Expect.equal (MultyValue <| [ Floating 10, Query <| Dict.fromList [(testStr, testStr)] ])
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect int" <|
                        \_ ->
                            "9.14f"
                                |> parser float
                                |> Expect.equal (Failure "could not convert string '9.14f' to a Float")
                    , test "Incorrect separator between floats" <|
                        \_ ->
                            "10?43"
                                |> parser (float </> float)
                                |> Expect.equal ( Failure <| "10?43 does not contain /")
                    , test "Incorrect float after ordered devider" <|
                        \_ ->
                            "5?a3"
                                |> parser (float <?> float)
                                |> Expect.equal ( Failure "could not convert string 'a3' to a Float" )
                    , test "Incorrect float after unordered divider" <|
                        \_ ->
                            "10&a43.0"
                                |> parser ( float <&> float )
                                |> Expect.equal ( Failure <| "Start of a43.0 does not have any value which can be corectly parsed by: Float, separated by &." )
                    , test "Incorrect ordered devider between paths" <|
                        \_ ->
                            let
                                path = "10.0/34"
                            in
                                path
                                    |> parser (float <?> float)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between paths" <|
                        \_ ->
                            let
                                path = "10&23.2"
                            in
                                path
                                    |> parser (float <*> float)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Float or Float, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between paths" <|
                        \_ ->
                            let
                                path = "10.1/2.4"
                            in
                                path
                                    |> parser (float <&> float)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Float or Float, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between paths" <|
                        \_ ->
                            let
                                path = "10.2*20.5"
                            in
                                path
                                    |> parser (float </> float)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            , describe "Str"
                [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            testStr
                                |> parser str 
                                |> Expect.equal ( Str testStr ) 
                    , describe "Ordered" <|
                        [ test "two string" <|
                            \_ ->
                                let
                                    testStr1 = testStr ++ "1"
                                    testStr2 = testStr ++ "2"
                                in
                                    testStr1 ++ "/" ++ testStr2
                                        |> parser  (str </> str) 
                                        |> Expect.equal ( MultyValue <| Str testStr1 :: Str testStr2 :: [] )
                        , test "string and path" <|
                            \_ ->
                                let
                                    testStr1 = testStr ++ "1"
                                    testStr2 = testStr ++ "2"
                                in
                                    testStr1 ++ "/" ++ testStr2
                                        |> parser (str </> p testStr2)
                                        |> Expect.equal ( Str testStr1 )
                        , test "string and int" <|
                            \_ ->
                                testStr ++ "/9"
                                    |> parser  (str </> int) 
                                    |> Expect.equal ( MultyValue <| Str testStr :: Interger 9 :: [] )
                        , test "string and float" <|
                            \_ ->
                                testStr ++ "/3.1415"
                                    |> parser  (str </> float) 
                                    |> Expect.equal ( MultyValue <| Str testStr :: Floating 3.1415 :: [] )
                        , test "string and any" <|
                            \_ ->
                                let 
                                    testStr1 = testStr ++ "1"
                                    testStr2 = testStr ++ "2"
                                in
                                testStr1 ++ "/" ++ testStr2
                                    |> parser  (str </> any) 
                                    |> Expect.equal ( Str testStr1 )
                        , test "string and query" <|
                            \_ ->
                                let
                                    testStr1 = testStr ++ "1"
                                    testStr2 = testStr ++ "2"
                                    testStr3 = testStr ++ "3"
                                in 
                                testStr1 ++ "/" ++ testStr2 ++ "=" ++ testStr3
                                    |> parser (str </> query)
                                    |> Expect.equal ( MultyValue 
                                        [ Str testStr1
                                        , Query <| Dict.fromList [(testStr2, testStr3)]
                                        ]
                                    )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "string or path" <|
                                \_ ->
                                    let
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr1 ++ "&" ++ testStr2
                                            |> parser (str <&> p testStr2)
                                            |> Expect.equal ( Str testStr1 ) 
                            , test "string or float" <|
                                \_ ->
                                    testStr ++ "&3.1415"
                                        |> parser (str <&> float )
                                        |> Expect.equal ( MultyValue <| [Str testStr, Floating 3.1415] )
                            , test "string or int" <|
                                \_ ->
                                    testStr ++ "*10"
                                        |> parser (str <*> int)
                                        |> Expect.equal ( MultyValue <| [Str testStr, Interger 10] )
                            , test "string or any" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in    
                                        testStr1 ++ "&" ++ testStr2
                                            |> parser ( str <&> any)
                                            |> Expect.equal ( Str testStr1 )
                            , test "string or query" <|
                                \_ ->
                                    let
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                        testStr3 = testStr ++ "3"
                                    in
                                    testStr1 ++ "&" ++ testStr2 ++ "=" ++ testStr3
                                        |> parser ( str <&> query )
                                        |> Expect.equal (MultyValue <| [ Str testStr1, Query <| Dict.fromList [(testStr2, testStr3)] ])
                            ]
                        , describe "inverted"
                            [ describe "limitations"
                                [ test "string or path" <|
                                    \_ ->
                                        let
                                            testStr1 = testStr ++ "1"
                                            testStr2 = testStr ++ "2"
                                        in
                                            testStr2 ++ "&" ++ testStr1
                                                |> parser (str <&> p testStr2)
                                                |> Expect.equal ( Failure <| "Start of " ++ testStr1 ++ " does not have any value which can be corectly parsed by: Path " ++ testStr2 ++ ", separated by &." )
                                , test "string or float" <|
                                    \_ ->
                                        "3.1415&" ++ testStr
                                            |> parser (str <&> float )
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr ++ " does not have any value which can be corectly parsed by: Float, separated by &.")
                                , test "string or int" <|
                                    \_ ->
                                        "10*" ++ testStr
                                            |> parser (str <*> int)
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr ++ " does not have any value which can be corectly parsed by: Int, separated by *." )
                                , test "string or any" <|
                                    \_ ->
                                        "10&" ++ testStr
                                            |> parser ( str <&> any)
                                            |> Expect.equal ( Str "10" )
                                , test "string or query" <|
                                    \_ ->
                                        let
                                            testStr1 = testStr ++ "1"
                                            testStr2 = testStr ++ "2"
                                            testStr3 = testStr ++ "3"
                                        in
                                        testStr2 ++ "=" ++ testStr3 ++ "&" ++ testStr1
                                            |> parser ( str <&> query )
                                            |> Expect.equal (Failure <| "Start of " ++ testStr1 ++ " does not have any value which can be corectly parsed by: Query, separated by &.")
                                ]
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect separator between values" <|
                        \_ ->
                            let
                                path = testStr ++ "?" ++ testStr
                            in
                                path
                                    |> parser (str </> str)
                                    |> Expect.equal ( Failure <| path ++ " does not contain /")
                    , test "Incorrect ordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "/" ++ testStr
                            in
                                path
                                    |> parser (str <?> str)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "&" ++ testStr
                            in
                                path
                                    |> parser (str <*> str)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: String or String, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "/" ++ testStr
                            in
                                path
                                    |> parser (str <&> str)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: String or String, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "*" ++ testStr
                            in
                                path
                                    |> parser (str </> str)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            , describe "Any"
                  [ describe "Correct"
                    [ test "single" <|
                        \_ ->
                            testStr
                                |> parser any 
                                |> Expect.equal ( Succes ) 
                    , describe "Ordered" <|
                        [ test "two any" <|
                            \_ ->
                                let
                                    testStr1 = testStr ++ "1"
                                    testStr2 = testStr ++ "2"
                                in
                                    testStr1 ++ "/" ++ testStr2
                                        |> parser  (any </> any) 
                                        |> Expect.equal ( Succes )
                        , test "any and path" <|
                            \_ ->
                                let
                                    testStr1 = testStr ++ "1"
                                    testStr2 = testStr ++ "2"
                                in
                                    testStr1 ++ "/" ++ testStr2
                                        |> parser (any </> p testStr2)
                                        |> Expect.equal ( Succes )
                        , test "any and int" <|
                            \_ ->
                                testStr ++ "/9"
                                    |> parser  (any </> int) 
                                    |> Expect.equal ( Interger 9 )
                        , test "any and float" <|
                            \_ ->
                                testStr ++ "/3.1415"
                                    |> parser  (any </> float) 
                                    |> Expect.equal ( Floating 3.1415 )
                        , test "any and string" <|
                            \_ ->
                                let 
                                    testStr1 = testStr ++ "1"
                                    testStr2 = testStr ++ "2"
                                in
                                testStr1 ++ "/" ++ testStr2
                                    |> parser  (any </> str) 
                                    |> Expect.equal ( Str testStr2 )
                        , test "any and query" <|
                            \_ ->
                                let
                                    testStr1 = testStr ++ "1"
                                    testStr2 = testStr ++ "2"
                                    testStr3 = testStr ++ "3"
                                in 
                                testStr1 ++ "/" ++ testStr2 ++ "=" ++ testStr3
                                    |> parser (any </> query)
                                    |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr3)] )
                        ]
                    , describe "Unordered" <|
                        [ describe "straight"
                            [ test "any or path" <|
                                \_ ->
                                    let
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in
                                        testStr1 ++ "&" ++ testStr2
                                            |> parser (any <&> p testStr2)
                                            |> Expect.equal ( Succes ) 
                            , test "any or float" <|
                                \_ ->
                                    testStr ++ "&3.1415"
                                        |> parser (any <&> float )
                                        |> Expect.equal ( Floating 3.1415 )
                            , test "any or int" <|
                                \_ ->
                                    testStr ++ "*10"
                                        |> parser (any <*> int)
                                        |> Expect.equal ( Interger 10 )
                            , test "any or string" <|
                                \_ ->
                                    let 
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                    in    
                                        testStr1 ++ "&" ++ testStr2
                                            |> parser ( any <&> str)
                                            |> Expect.equal ( Str testStr2 )
                            , test "any or query" <|
                                \_ ->
                                    let
                                        testStr1 = testStr ++ "1"
                                        testStr2 = testStr ++ "2"
                                        testStr3 = testStr ++ "3"
                                    in
                                        testStr1 ++ "&" ++ testStr2 ++ "=" ++ testStr3
                                            |> parser ( any <&> query )
                                            |> Expect.equal ( Query <| Dict.fromList [(testStr2, testStr3)] )
                            ]
                        , describe "inverted"
                            [ describe "limitations"
                                [ test "any or path" <|
                                    \_ ->
                                        let
                                            testStr1 = testStr ++ "1"
                                            testStr2 = testStr ++ "2"
                                        in
                                            testStr2 ++ "&" ++ testStr1
                                                |> parser (any <&> p testStr2)
                                                |> Expect.equal ( Failure <| "Start of " ++ testStr1 ++ " does not have any value which can be corectly parsed by: Path " ++ testStr2 ++ ", separated by &." )
                                , test "any or float" <|
                                    \_ ->
                                        "3.1415&" ++ testStr
                                            |> parser (any <&> float )
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr ++ " does not have any value which can be corectly parsed by: Float, separated by &.")
                                , test "any or int" <|
                                    \_ ->
                                        "10*" ++ testStr
                                            |> parser (any <*> int)
                                            |> Expect.equal ( Failure <| "Start of " ++ testStr ++ " does not have any value which can be corectly parsed by: Int, separated by *." )
                                , test "any or query" <|
                                    \_ ->
                                        let
                                            testStr1 = testStr ++ "1"
                                            testStr2 = testStr ++ "2"
                                            testStr3 = testStr ++ "3"
                                        in
                                        testStr2 ++ "=" ++ testStr3 ++ "&" ++ testStr1
                                            |> parser ( any <&> query )
                                            |> Expect.equal (Failure <| "Start of " ++ testStr1 ++ " does not have any value which can be corectly parsed by: Query, separated by &.")
                                ]
                            , test "any or string" <|
                                \_ ->
                                    "10&" ++ testStr
                                        |> parser ( any <&> str)
                                        |> Expect.equal ( Str testStr )
                            ]
                        ]
                    ]
                , describe "Error"
                    [ test "Incorrect separator between values" <|
                        \_ ->
                            let
                                path = testStr ++ "?" ++ testStr
                            in
                                path
                                    |> parser (any </> any)
                                    |> Expect.equal ( Failure <| path ++ " does not contain /")
                    , test "Incorrect ordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "/" ++ testStr
                            in
                                path
                                    |> parser (any <?> any)
                                    |> Expect.equal (Failure <| path ++ " does not contain ?" )   
                    , test "Incorrect unordered devider between values" <|
                        \_ ->
                            let
                                path = testStr ++ "&" ++ testStr
                            in
                                path
                                    |> parser (any <*> any)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Any or Any, separated by *." )     
                    , test "Incorrect unordered devider instead of ordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "/" ++ testStr
                            in
                                path
                                    |> parser (any <&> any)
                                    |> Expect.equal (Failure <| "Start of " ++ path ++ " does not have any value which can be corectly parsed by: Any or Any, separated by &."  )     
                    , test "Incorrect ordered devider instead of unordered one between values" <|
                        \_ ->
                            let
                                path = testStr ++ "*" ++ testStr
                            in
                                path
                                    |> parser (any </> any)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )      
                    ]
                ]
            , describe "Query"
                [ describe "Correct"
                    [ test "just single short query" <|
                        \_ ->
                            testStr ++ "=" ++ testStr   
                                |> parser query
                                |> Expect.equal ( Query <| Dict.fromList [(testStr, testStr)] )
                    , test "just single long query" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                            in
                                str1 ++ "=" ++ testStr ++ "&" ++ str2 ++ "=" ++ testStr
                                    |> parser query
                                    |> Expect.equal ( Query <| Dict.fromList [(str1, testStr), (str2, testStr)] )
                    , test "two query" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                            in
                                str1 ++ "=" ++ testStr ++ "/" ++ str2 ++ "=" ++ testStr
                                    |> parser ( query </> query )
                                    |> Expect.equal ( MultyValue
                                        [ Query <| Dict.fromList [(str1, testStr)] 
                                        , Query <| Dict.fromList [(str2, testStr)] 
                                        ]
                                    )
                    , test "query and int" <|
                        \_ ->
                            testStr ++ "=" ++ testStr ++ "/10"
                                |> parser (query </> int)
                                |> Expect.equal ( MultyValue
                                    [ Query <| Dict.fromList [(testStr, testStr)]
                                    , Interger 10
                                    ]
                                ) 
                    , test "query and float" <|
                        \_ ->
                            testStr ++ "=" ++ testStr ++"/3.1415"
                                |> parser (query </> float)
                                |> Expect.equal ( MultyValue
                                    [ Query <| Dict.fromList [(testStr, testStr)]
                                    , Floating 3.1415
                                    ]
                                )
                    , test "query and string" <|
                        \_ ->
                            testStr ++ "=" ++ testStr ++ "/" ++ testStr
                                |> parser (query </> str)
                                |> Expect.equal ( MultyValue
                                    [ Query <| Dict.fromList [(testStr, testStr)]
                                    , Str testStr
                                    ]
                                )
                    , test "query and path" <|
                        \_ ->
                            testStr ++ "=" ++ testStr ++ "/" ++ testStr
                                |> parser (query </>  p testStr)
                                |> Expect.equal ( Query <| Dict.fromList [(testStr, testStr)] )
                    ]
                , describe "Error" 
                    [ test "Incorrect query" <|
                        \_ ->
                            testStr
                                |> parser ( query)
                                |> Expect.equal (Failure <| "Query is not correct: string does not contain =" )
                    , test "Incorrect value in long query" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                            in
                                str1 ++ "=" ++ testStr ++ "&" ++ str2
                                    |> parser query
                                    |> Expect.equal ( Failure "Query is not correct: string2 does not contain =")
                    , test "Incorrect second query" <|
                        \_ ->
                            testStr ++ "=" ++ testStr ++ "/" ++ testStr 
                                |> parser (query </> query)
                                |> Expect.equal ( Failure "Query is not correct: string does not contain =" )    
                    , test "Incorrect devider between query" <|
                        \_ ->
                            let
                                str1 = testStr ++ "1"
                                str2 = testStr ++ "2"
                                path = testStr ++ "=" ++ testStr ++ "?" ++ testStr ++ "=" ++ testStr
                            in
                                path
                                    |> parser ( query </> query)
                                    |> Expect.equal (Failure <| path ++ " does not contain /" )     
                    ]
                ]
            ]
        ]
        