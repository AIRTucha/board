module Board.Router.InternalsTest exposing (..)

import Ordeal exposing (Test, test, describe, shouldEqual, andTest, failure, all, and)
import Board.Router exposing (empty)
import Board.Router.Internals exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any, str, int)
import Board.Internals exposing(..)
import Tuple exposing (second)
import List exposing (concat, map)
import Task exposing (succeed, andThen)
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


model =
    "model"


eqaulValues sync1 sync2 =
     case sync1 of
        StateLess value1 ->
            case sync2 of
                StateLess value2 ->
                    shouldEqual value1 value2

                StateFull value2 ->
                    failure "The first arg is stateless, while second one is statefull"

        StateFull value1 ->
            case sync2 of
                StateLess value2 ->
                    failure "The first arg is stateless, while second one is statefull"

                StateFull value2 ->
                    let 
                        (model1, answer1) = value1 model
                        (model2, answer2) = value2 model
                        areModelsEqual = shouldEqual model1 model2
                        areAnswersEqual = equal answer1 answer2
                    in  
                        and areModelsEqual areAnswersEqual

equal v1 v2 =
    case v1 of
        Sync sync1 ->
            case v2 of
                Sync sync2 ->
                   eqaulValues sync1 sync2

                Async async2 ->
                    failure "The first arg is sync, while second one is async"

        Async async1 ->
            case v2 of
                Sync sync2 ->
                    failure "The first arg is async, while second one is sync"

                Async async2 ->
                    async1
                        |> andThen ( \ sync1 -> async2 |> Task.map (\ sync2 -> (sync1, sync2) ))
                        |> andTest (\ result ->
                            case result of 
                                Ok (value1, value2) ->
                                    eqaulValues value1 value2
                                
                                Err str ->
                                    failure str
                        )


testCheckerAndMethod (chekcer, method, name) =
    let 
        req = request method
        stateLessSyncResult = 
            result chekcer req stateLessSyncNext stateLessSync
        stateLessAsyncResult handler _ = 
            result chekcer req (StateLess << Next) StateLess handler
        toStateHandler reqToValue req str model =
           (model, stateLessSync <| reqToValue req str )
        stateFullSyncRouter reqToValue req = 
            stateFullSync <| toStateHandler reqToValue req req.url
        stateFullSyncNext =
            stateFullSyncRouter next
        stateFullSyncResult reqToValue =
            result chekcer req stateFullSyncNext stateFullSync <| toStateHandler reqToValue
        stateFullAsyncRouter reqToValue req = 
            stateFullAsync <| toStateHandler reqToValue req req.url
        stateFullAsyncNext =
            stateFullAsyncRouter next
        stateFullAsyncResult reqToValue =
            result chekcer req stateFullAsyncNext stateFullAsync <| toStateHandler reqToValue
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
                            rout req = req.url
                                |> Redirect
                                |> stateLessSync 
                        in
                            syncRouter chekcer any toNext rout req
                                |> shouldEqual (rout req)
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
                            rout = getResponse >> Reply >> StateLess
                        in
                            syncRouter chekcer str toNext (rout >> succeed >> Async) req
                                |> testTask rout req
                    )
                    , test "Router Redirect" (
                        let
                            redirect req = req.url
                                |> Redirect
                                |> StateLess
                            rout = redirect >> succeed >> Async
                        in
                            syncRouter chekcer str toNext rout req
                                |> testTask redirect req
                    )
                    , describe "Router Next"
                        [ test "Handler Redirect" (
                            syncRouter chekcer str toRedirect stateLessAsyncNext req
                                |> testTask (stateLessAsyncResult redirect ) req
                        )
                        , test "Handler Reply" (
                                syncRouter chekcer str toResponse stateLessAsyncNext req
                                    |> testTask (stateLessAsyncResult response ) req
                        )
                        , test "Handler Next" (
                                syncRouter chekcer str toNext stateLessAsyncNext req
                                    |> testTask (stateLessAsyncResult next) req  
                        )
                        , test "Hanler URL does not match" (
                                syncRouter chekcer int toNext stateLessAsyncNext req
                                    |> testTask (StateLess << Next) req  
                        )
                        ]
                    ]
                , describe "Sync State Router"
                    [ test "Router Response" (   
                        let
                            rout = 
                                stateFullSyncRouter response
                        in
                            syncRouter chekcer any toNext rout req
                                |> equal (rout req)
                        )
                    , test "Router Redirect" (
                        let
                            rout = 
                                stateFullSyncRouter redirect
                        in
                            syncRouter chekcer any toNext rout req
                                |> equal (rout req)
                    )
                    , describe "Router Next"
                        [ test "Handler Redirect" ( 
                            syncRouter chekcer str toRedirect stateFullSyncNext req
                                |> equal (stateFullSyncResult redirect)
                        )
                        , test "Handler Reply" ( 
                            syncRouter chekcer str toResponse stateFullSyncNext req
                                |> equal (stateFullSyncResult response)
                        ) 
                        , test "Handler Next" (
                            syncRouter chekcer str toNext stateFullSyncNext req
                                |> equal (stateFullSyncResult next)
                        ) 
                        , test "Hanler URL does not match" ( 
                            syncRouter chekcer int toNext stateFullSyncNext req
                                |> equal (stateFullSyncNext req)
                        )
                        ]
                    ]
                , describe "Async State Router"
                    [ test "Router Response" (   
                        let
                            rout = 
                                stateFullAsyncRouter response
                        in
                            syncRouter chekcer any toNext rout req
                                |> equal (rout req)
                        )
                    , test "Router Redirect" (
                        let
                            rout = 
                                stateFullAsyncRouter redirect
                        in
                            syncRouter chekcer any toNext rout req
                                |> equal (rout req)
                    )
                    , describe "Router Next"
                        [ test "Handler Redirect" ( 
                            syncRouter chekcer str toRedirect stateFullAsyncNext req
                                |> equal (stateFullAsyncResult redirect)
                        )
                        , test "Handler Reply" ( 
                            syncRouter chekcer str toResponse stateFullAsyncNext req
                                |> equal (stateFullAsyncResult response)
                        ) 
                        , test "Handler Next" (
                            syncRouter chekcer str toNext stateFullAsyncNext req
                                |> equal (stateFullAsyncResult next)
                        ) 
                        , test "Hanler URL does not match" ( 
                            syncRouter chekcer int toNext stateFullAsyncNext req
                                |> equal (stateFullAsyncNext req)
                        )
                        ]
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
            , describe "Async Handler"
                [ describe "Async Router" 
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
                                |> testTask (stateLessAsyncResult redirect ) req
                        )
                        , test "Handler Reply" (
                                asyncRouter chekcer str (toResponse >> succeed) stateLessAsyncNext req
                                    |> testTask (stateLessAsyncResult response ) req
                        )
                        , test "Handler Next" (
                                asyncRouter chekcer str (toNext >> succeed) stateLessAsyncNext req
                                    |> testTask (stateLessAsyncResult next) req  
                        )
                        , test "Hanler URL does not match" (
                                asyncRouter chekcer int (toNext >> succeed) stateLessAsyncNext req
                                    |> testTask (StateLess << Next) req  
                        )
                        ]
                    ]
                ]
            ]

{-
-}
routerInternals : Test
routerInternals =
    describe "Router Interlan logic" (map testCheckerAndMethod methodTestCases)
