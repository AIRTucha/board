module Board.Router.InternalsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
import Board.Router exposing (empty)
import Board.Router.Internals exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any, str, int)
import Board.Internals exposing(..)
import Tuple exposing (second)
import List exposing (concat, map)

methodCheckers = 
    [ ( isGet, "Get" )
    , ( isPost, "Post" )
    , ( isPut, "Put" )
    , ( isDelete, "Delete" )
    , ( anyMethod, "Any" )
    ]


methods = 
    [ ( Get, "Get" )
    , ( Post, "Post" )
    , ( Put, "Put" )
    , ( Delete, "Delete" )
    ]


jounMethodToChecker (checker, checkerName) (method, methodName) =
    (checker, method, "Expects " ++ checkerName ++ ", received " ++ methodName)


joinCheckerToMethods checkerTuple =
    methods
        |> map (jounMethodToChecker checkerTuple)


methodTestCases =
    methodCheckers
        |> map joinCheckerToMethods
        |> concat


url = "test"


request method =
    let 
        req = getRequest method
    in 
        { req | url = url }


response req str = 
    let 
        res = getResponse req
    in 
        Reply { res | content = Text "text/plain" str }


next req str =
    Next { req | content = Text "text/plain" str }


redirect req str =
    Redirect str


getHandler hanlder (param, req) =
    case param of
        StrParam str ->
            hanlder req str
        
        _ -> 
            Next req


toResponse =
    getHandler response


toNext = 
    getHandler next


toRedirect =
    getHandler redirect


stateLessSyncNext = 
    Next >> stateLessSync


result checker req mismatchHandler matchHandler matchMode =
    if checker req then 
        matchMode <| matchHandler req url
    else 
        mismatchHandler req
    

testCheckerAndMethod (chekcer, method, name) =
    let 
        req = request method
        stateLessSyncResult = 
            result chekcer req stateLessSyncNext
    in
        describe name
            [ describe "Sync Handler"
                [ describe "Sync Router" 
                    [ test "Router Response" <|
                        \_ -> 
                            let
                                rout = getResponse >> Reply >> stateLessSync
                            in
                                syncRouter chekcer any toNext rout req
                                    |> Expect.equal (rout req)
                    , test "Router Redirect" <|
                        \_ ->
                            let
                                redirect = "test"
                                    |> Redirect
                                    |> stateLessSync 
                                rout = \_ -> redirect
                            in
                                syncRouter chekcer any toNext rout req
                                    |> Expect.equal redirect
                    , describe "Router Next"
                        [ test "Handler Redirect" <|
                            \_ ->
                                syncRouter chekcer str toRedirect stateLessSyncNext req
                                    |> Expect.equal (stateLessSyncResult redirect stateLessSync )
                        , test "Handler Reply" <|
                            \_ ->
                                syncRouter chekcer str toResponse stateLessSyncNext req
                                    |> Expect.equal (stateLessSyncResult response stateLessSync )  
                        , test "Handler Next" <|
                            \_ ->
                                syncRouter chekcer str toNext stateLessSyncNext req
                                    |> Expect.equal (stateLessSyncResult next stateLessSync )   
                        , test "Hanler URL does not match" <|
                            \_ ->
                                syncRouter chekcer int toNext stateLessSyncNext req
                                    |> Expect.equal (stateLessSyncNext req)  
                        ]
                    ]
                ]
            , describe "Sync State Router"
                [ 
                    test "Pass" <|
                        \_ -> 
                            let
                                handler = \(params, req) -> Next req 
                                req = getRequest Get
                            in
                                router stateLessSync anyMethod any handler empty req
                                    |> Expect.equal (nextStateLessSync req)
                ]
            -- Different handlers
                -- Different router types
                    -- Check router respons
                    -- Check router redirect
                    -- Check router next
                        -- Check that handler redirect
                        -- Check that handler next
                        -- Check that handler respons
                        -- Check URL fails
            ]

{-
-}
param : Test
param =
    describe "Router Interlan logic" (map testCheckerAndMethod methodTestCases)
