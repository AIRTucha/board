module Board.Router.InternalsTest exposing (..)

import Ordeal exposing (Test, test, describe, shouldEqual, andTest, failure)
import Board.Router exposing (empty)
import Board.Router.Internals exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any, str, int)
import Board.Internals exposing(..)
import Tuple exposing (second)
import List exposing (concat, map)
import Task exposing (succeed)
import Debug 

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


stateLessAsyncNext = 
    Next >> succeed >> stateLessAsync


result checker req mismatchHandler matchMode matchHandler =
    if checker req then 
        matchMode <| matchHandler req url
    else 
        mismatchHandler req


testTask reqToResult req result = 
    let 
        checkResult result = 
            case result of 
                Ok value ->
                    shouldEqual value (reqToResult req)

                Err str ->
                    failure str
    in
        case result of
            Sync _ ->
                failure "Incorrect synchronization mode"

            Async task ->
                task
                    |> andTest checkResult


testCheckerAndMethod (chekcer, method, name) =
    let 
        req = request method
        stateLessSyncResult = 
            result chekcer req stateLessSyncNext stateLessSync
        stateLessAsyncResult = 
            result chekcer req stateLessSyncNext
    in
        describe name
            [ describe "Sync Handler"
                [ describe "Sync Router" 
                    [ test "Router Response" (   
                        let
                            rout = getResponse >> Reply >> stateLessSync
                        in
                            syncRouter chekcer any toNext rout req
                                |> shouldEqual (rout req)
                    )
                    , test "Router Redirect" (
                        let
                            redirect = url
                                |> Redirect
                                |> stateLessSync 
                            rout _ = redirect
                        in
                            syncRouter chekcer any toNext rout req
                                |> shouldEqual redirect
                    )
                    , describe "Router Next"
                        [ test "Handler Redirect" ( 
                            syncRouter chekcer str toRedirect stateLessSyncNext req
                                |> shouldEqual (stateLessSyncResult redirect )
                        )
                        , test "Handler Reply" ( 
                            syncRouter chekcer str toResponse stateLessSyncNext req
                                |> shouldEqual (stateLessSyncResult response ) 
                        ) 
                        , test "Handler Next" (
                                syncRouter chekcer str toNext stateLessSyncNext req
                                    |> shouldEqual (stateLessSyncResult next )  
                        ) 
                        , test "Hanler URL does not match" ( 
                            syncRouter chekcer int toNext stateLessSyncNext req
                                    |> shouldEqual (stateLessSyncNext req)  
                        )
                        ]
                    ]
                ]
                , describe "Async Router" 
                    [ test "Router Response" (
                        let
                            syncRouter = getResponse >> Reply >> StateLess
                            rout = syncRouter >> succeed >> Async
                        in
                            asyncRouter chekcer str ( toNext >> succeed ) rout req
                                |> testTask syncRouter req
                    )
                    , test "Router Redirect" (
                        let
                            redirect = url
                                |> Redirect
                                |> StateLess
                            rout _ = (succeed >> Async) redirect
                        in
                            asyncRouter chekcer str ( toNext >> succeed ) rout req
                                |> testTask (\_ -> redirect) req
                    )
                    , describe "Router Next"
                        [ test "Handler Redirect" (
                            asyncRouter chekcer str (toRedirect >> succeed) stateLessAsyncNext req
                                |> testTask ( \_ -> url |> Redirect |> StateLess ) req
                        )
                    --     , test "Handler Reply" <|
                    --         \_ ->
                    --             syncRouter chekcer str toResponse stateLessSyncNext req
                    --                 |> shouldEqual (stateLessSyncResult response stateLessSync )  
                    --     , test "Handler Next" <|
                    --         \_ ->
                    --             syncRouter chekcer str toNext stateLessSyncNext req
                    --                 |> shouldEqual (stateLessSyncResult next stateLessSync )   
                    --     , test "Hanler URL does not match" <|
                    --         \_ ->
                    --             syncRouter chekcer int toNext stateLessSyncNext req
                    --                 |> shouldEqual (stateLessSyncNext req)  
                        ]
                    ]
                , describe "Sync State Router"
                    [ 
                        test "Pass" (   
                            let
                                handler (params, req) = Next req 
                                req = getRequest Get
                            in
                                router stateLessSync anyMethod any handler empty req
                                    |> shouldEqual (nextStateLessSync req)
                        )    
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
routerInternals : Test
routerInternals =
    describe "Router Interlan logic" (map testCheckerAndMethod methodTestCases)
