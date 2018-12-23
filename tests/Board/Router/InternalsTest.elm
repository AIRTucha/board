module Board.Router.InternalsTest exposing (..)

import Ordeal exposing (Test, success, test, describe, shouldEqual, andTest, failure, all, and)
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

redirect: a -> String -> AnswerValue value model error
redirect req str =
    Redirect str

stateFullRouter resp req str model =
    (model, stateLessSync <| resp req str)


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


toResponseTask =
    toResponse >> succeed


toNextTask =  
    toNext >> succeed


toRedirectTask =
    toRedirect >> succeed


stateLessSyncNext = 
    Next >> stateLessSync


stateLessAsyncNext = 
    Next >> succeed >> stateLessAsync


stateLessNext =
    Next >> StateLess
    
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
                    eqaulValues value (reqToResult req)

                Err str ->
                    failure str
    in
        result
            |> liftToAsync
            |> andTest checkResult


model =
    "model"


eqaulValues sync1 sync2 =
     case Debug.log "ev1" sync1 of
        StateLess value1 ->
            case  Debug.log "ev2" sync2 of
                StateLess value2 ->
                    shouldEqual value1 value2

                StateFull value2 ->
                    failure "The first arg is stateless, while second one is statefull"

        StateFull value1 ->
            case  Debug.log "ev2" sync2 of
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
    case Debug.log "v1" v1 of
        Sync sync1 ->
            case Debug.log "v2" v2 of
                Sync sync2 ->
                   eqaulValues sync1 sync2

                Async async2 ->
                    failure "The first arg is sync, while second one is async"

        Async async1 ->
            case Debug.log "v2" v2 of
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


toStateFullHanlder handler args model =
    (model, handler args)


stateFullNext = 
    toStateFullHanlder toNext


stateFullRedirect =
    toStateFullHanlder toRedirect


stateFullResponse =
    toStateFullHanlder toResponse


testCheckerAndMethod (chekcer, method, name) =
    let 
        req = request method
        stateLessSyncResult = 
            result chekcer req stateLessSyncNext stateLessSync
        stateFulSyncResult = 
            toSyncStateHandler >> result chekcer req stateLessSyncNext stateFullSync 
        stateFulAsyncResult = 
            toSyncStateHandler >> result chekcer req stateLessSyncNext stateFullAsync
        stateLessAsyncResult handler _ = 
            result chekcer req stateLessNext StateLess handler
        stateFullAsyncResult handler _ = 
            result chekcer req stateLessNext StateFull (toSyncStateHandler handler)
        toStateFulRouter mode reqToValue req str model =
           (model, mode <| reqToValue req str )
        toSyncStateHandler =
           toStateFulRouter stateLessSync
        toSyncStateHandlerStateFull = 
           toStateFulRouter stateFullSync
        toAsyncStateHandler =
           toStateFulRouter (stateLessAsync << succeed)
        stateFullSyncRouter reqToValue req = 
            stateFullSync <| toSyncStateHandler reqToValue req req.url
        stateFullSyncNext =
            stateFullSyncRouter next
        stateFullSyncResult reqToValue =
            result chekcer req stateFullSyncNext stateFullSync <| toSyncStateHandler reqToValue
        stateFullSyncResultStateFullHanlder reqToValue =
            result chekcer req stateFullSyncNext stateFullSync <| toSyncStateHandlerStateFull reqToValue
        stateFullAsyncResultStateFullHanlder reqToValue =
            result chekcer req stateFullAsyncNext stateFullAsync <| (toStateFulRouter stateFullSync (\ a s m -> (m, stateLessSync <| reqToValue a s))) 
        stateFullAsyncRouter reqToValue req = 
            stateFullAsync <| toSyncStateHandler reqToValue req req.url
        stateFullAsyncNext =
            stateFullAsyncRouter next
        asyncStateFullAsyncResultSync reqToValue =
            result chekcer req stateFullAsyncNext stateFullAsync <| toSyncStateHandler reqToValue
        syncStateFullSyncResultAsync reqToValue =
            result chekcer req stateFullSyncNext stateFullSync <| toAsyncStateHandler reqToValue
        asyncStateFullAsyncResultAsync reqToValue =
            result chekcer req stateFullAsyncNext stateFullAsync <| toAsyncStateHandler reqToValue
    in
        describe name
            [ 
                -- describe "Sync StateLess Handler"
                -- [ describe "Sync Router" 
                --     [ test "Router Response" (   
                --         let
                --             rout = getResponse >> Reply >> stateLessSync
                --         in
                --             syncRouter chekcer any toNext rout req
                --                 |> shouldEqual (rout req)
                --     )
                --     , test "Router Redirect" (
                --         let
                --             rout req = req.url
                --                 |> Redirect
                --                 |> stateLessSync 
                --         in
                --             syncRouter chekcer any toNext rout req
                --                 |> shouldEqual (rout req)
                --     )
                --     , describe "Router Next"
                --         [ test "Handler Redirect" ( 
                --             syncRouter chekcer str toRedirect stateLessSyncNext req
                --                 |> shouldEqual (stateLessSyncResult redirect )
                --         )
                --         , test "Handler Reply" ( 
                --             syncRouter chekcer str toResponse stateLessSyncNext req
                --                 |> shouldEqual (stateLessSyncResult response ) 
                --         ) 
                --         , test "Handler Next" (
                --                 syncRouter chekcer str toNext stateLessSyncNext req
                --                     |> shouldEqual (stateLessSyncResult next )  
                --         ) 
                --         , test "Hanler URL does not match" ( 
                --             syncRouter chekcer int toNext stateLessSyncNext req
                --                     |> shouldEqual (stateLessSyncNext req)  
                --         )
                --         ]
                --     ]
                -- , describe "Async Router" 
                --     [ test "Router Response" (
                --         let
                --             rout = getResponse >> Reply >> StateLess
                --         in
                --             syncRouter chekcer str toNext (rout >> succeed >> Async) req
                --                 |> testTask rout req
                --     )
                --     , test "Router Redirect" (
                --         let
                --             redirect req = req.url
                --                 |> Redirect
                --                 |> StateLess
                --             rout = redirect >> succeed >> Async
                --         in
                --             syncRouter chekcer str toNext rout req
                --                 |> testTask redirect req
                --     )
                --     , describe "Router Next"
                --         [ test "Handler Redirect" (
                --             syncRouter chekcer str toRedirect stateLessAsyncNext req
                --                 |> testTask (stateLessAsyncResult redirect ) req
                --         )
                --         , test "Handler Reply" (
                --                 syncRouter chekcer str toResponse stateLessAsyncNext req
                --                     |> testTask (stateLessAsyncResult response ) req
                --         )
                --         , test "Handler Next" (
                --                 syncRouter chekcer str toNext stateLessAsyncNext req
                --                     |> testTask (stateLessAsyncResult next) req  
                --         )
                --         , test "Hanler URL does not match" (
                --                 syncRouter chekcer int toNext stateLessAsyncNext req
                --                     |> testTask stateLessNext req  
                --         )
                --         ]
                --     ]
                -- , describe "Sync State Router"
                --     [ test "Router Response" (   
                --         let
                --             rout = 
                --                 stateFullSyncRouter response
                --         in
                --             syncRouter chekcer any toNext rout req
                --                 |> equal (rout req)
                --         )
                --     , test "Router Redirect" (
                --         let
                --             rout = 
                --                 stateFullSyncRouter redirect
                --         in
                --             syncRouter chekcer any toNext rout req
                --                 |> equal (rout req)
                --     )
                --     , describe "Router Next"
                --         [ test "Handler Redirect" ( 
                --             syncRouter chekcer str toRedirect stateFullSyncNext req
                --                 |> equal (stateFullSyncResult redirect)
                --         )
                --         , test "Handler Reply" ( 
                --             syncRouter chekcer str toResponse stateFullSyncNext req
                --                 |> equal (stateFullSyncResult response)
                --         ) 
                --         , test "Handler Next" (
                --             syncRouter chekcer str toNext stateFullSyncNext req
                --                 |> equal (stateFullSyncResult next)
                --         ) 
                --         , test "Hanler URL does not match" ( 
                --             syncRouter chekcer int toNext stateFullSyncNext req
                --                 |> equal (stateFullSyncNext req)
                --         )
                --         ]
                --     ]
                -- , describe "Async State Router"
                --     [ test "Router Response" (   
                --         let
                --             rout = 
                --                 stateFullAsyncRouter response
                --         in
                --             syncRouter chekcer any toNext rout req
                --                 |> equal (rout req)
                --         )
                --     , test "Router Redirect" (
                --         let
                --             rout = 
                --                 stateFullAsyncRouter redirect
                --         in
                --             syncRouter chekcer any toNext rout req
                --                 |> equal (rout req)
                --     )
                --     , describe "Router Next"
                --         [ test "Handler Redirect" ( 
                --             syncRouter chekcer str toRedirect stateFullAsyncNext req
                --                 |> equal (asyncStateFullAsyncResultSync redirect)
                --         )
                --         , test "Handler Reply" ( 
                --             syncRouter chekcer str toResponse stateFullAsyncNext req
                --                 |> equal (asyncStateFullAsyncResultSync response)
                --         ) 
                --         , test "Handler Next" (
                --             syncRouter chekcer str toNext stateFullAsyncNext req
                --                 |> equal (asyncStateFullAsyncResultSync next)
                --         ) 
                --         , test "Hanler URL does not match" ( 
                --             syncRouter chekcer int toNext stateFullAsyncNext req
                --                 |> equal (stateFullAsyncNext req)
                --         )
                --         ]
                --     ]
                -- ]
            -- , describe "Async StateLess Handler"
            --     [ describe "Sync Router" 
            --         [ test "Router Response" (   
            --             let
            --                 rout = getResponse >> Reply >> stateLessSync
            --             in
            --                 asyncRouter chekcer any toNextTask rout req
            --                     |> shouldEqual (rout req)
            --         )
            --         , test "Router Redirect" (
            --             let
            --                 rout req = req.url
            --                     |> Redirect
            --                     |> stateLessSync 
            --             in
            --                 asyncRouter chekcer any toNextTask rout req
            --                     |> shouldEqual (rout req)
            --         )
            --         , describe "Router Next"
            --             [ test "Handler Redirect" ( 
            --                 asyncRouter chekcer str toRedirectTask stateLessSyncNext req
            --                     |> testTask ( stateLessAsyncResult redirect ) req
            --             )
            --             , test "Handler Reply" ( 
            --                 asyncRouter chekcer str toResponseTask stateLessSyncNext req
            --                     |> testTask ( stateLessAsyncResult response ) req
            --             ) 
            --             , test "Handler Next" (
            --                 asyncRouter chekcer str toNextTask stateLessSyncNext req
            --                     |> testTask ( stateLessAsyncResult next )  req
            --             ) 
            --             , test "Hanler URL does not match" ( 
            --                 asyncRouter chekcer int toNextTask stateLessSyncNext req
            --                     |> testTask stateLessNext req  
            --             )
            --             ]
            --         ]
            --     , describe "Async Router" 
            --         [ test "Router Response" (
            --             let
            --                 syncRouter = getResponse >> Reply >> StateLess
            --                 rout = syncRouter >> succeed >> Async
            --             in
            --                 asyncRouter chekcer str toNextTask rout req
            --                     |> testTask syncRouter req
            --         )
            --         , test "Router Redirect" (
            --             let
            --                 redirect = url
            --                     |> Redirect
            --                     |> StateLess
            --                 rout _ = (succeed >> Async) redirect
            --             in
            --                 asyncRouter chekcer str toNextTask rout req
            --                     |> testTask (\_ -> redirect) req
            --         )
            --         , describe "Router Next"
            --             [ test "Handler Redirect" (
            --                 asyncRouter chekcer str toRedirectTask stateLessAsyncNext req
            --                     |> testTask (stateLessAsyncResult redirect ) req
            --             )
            --             , test "Handler Reply" (
            --                 asyncRouter chekcer str toResponseTask stateLessAsyncNext req
            --                     |> testTask (stateLessAsyncResult response ) req
            --             )
            --             , test "Handler Next" (
            --                 asyncRouter chekcer str toNextTask stateLessAsyncNext req
            --                     |> testTask (stateLessAsyncResult next) req  
            --             )
            --             , test "Hanler URL does not match" (
            --                 asyncRouter chekcer int toNextTask stateLessAsyncNext req
            --                     |> testTask stateLessNext req  
            --             )
            --             ]
            --         ]
            --     , describe "Sync State Router"
            --         [ test "Router Response" (   
            --             let
            --                 rout = 
            --                     stateFullSyncRouter response
            --             in
            --                 asyncRouter chekcer any toNextTask rout req
            --                     |> equal (rout req)
            --             )
            --         , test "Router Redirect" (
            --             let
            --                 rout = 
            --                     stateFullSyncRouter redirect
            --             in
            --                 asyncRouter chekcer any toNextTask rout req
            --                     |> equal (rout req)
            --         )
            --         , describe "Router Next"
            --             [ test "Handler Redirect" (
            --                 asyncRouter chekcer str toRedirectTask stateFullSyncNext req
            --                     |> equal (syncStateFullSyncResultAsync redirect)
            --             )
            --             , test "Handler Reply" ( 
            --                 asyncRouter chekcer str toResponseTask stateFullSyncNext req
            --                     |> equal (syncStateFullSyncResultAsync response)
            --             ) 
            --             , test "Handler Next" (
            --                 asyncRouter chekcer str toNextTask stateFullSyncNext req
            --                     |> equal (syncStateFullSyncResultAsync next)
            --             ) 
            --             , test "Hanler URL does not match" ( 
            --                 asyncRouter chekcer int toNextTask stateFullSyncNext req
            --                     |> equal (stateFullSyncNext req)
            --             )
            --             ]
            --         ]
            --     , describe "Async State Router"
            --         [ test "Router Response" (   
            --             let
            --                 rout = 
            --                     stateFullAsyncRouter response
            --             in
            --                 asyncRouter chekcer any toNextTask rout req
            --                     |> equal (rout req)
            --             )
            --         , test "Router Redirect" (
            --             let
            --                 rout = 
            --                     stateFullAsyncRouter redirect
            --             in
            --                 asyncRouter chekcer any toNextTask rout req
            --                     |> equal (rout req)
            --         )
            --         , describe "Router Next"
            --             [ test "Handler Redirect" ( 
            --                 asyncRouter chekcer str toRedirectTask stateFullAsyncNext req
            --                     |> equal (asyncStateFullAsyncResultAsync redirect)
            --             )
            --             , test "Handler Reply" ( 
            --                 asyncRouter chekcer str toResponseTask stateFullAsyncNext req
            --                     |> equal (asyncStateFullAsyncResultAsync response)
            --             ) 
            --             , test "Handler Next" (
            --                 asyncRouter chekcer str toNextTask stateFullAsyncNext req
            --                     |> equal (asyncStateFullAsyncResultAsync next)
            --             ) 
            --             , test "Hanler URL does not match" ( 
            --                 asyncRouter chekcer int toNextTask stateFullAsyncNext req
            --                     |> equal (stateFullAsyncNext req)
            --             )
            --             ]
            --         ]
            --     ]
            -- , describe "Sync StateFul Handler"
            --     [ describe "Sync Router" 
            --         [ test "Router Response" (   
            --             let
            --                 rout = getResponse >> Reply >> stateLessSync
            --             in
            --                 syncStateRouter chekcer any stateFullNext rout req
            --                     |> shouldEqual (rout req)
            --         )
            --         , test "Router Redirect" (
            --             let
            --                 rout req = req.url
            --                     |> Redirect
            --                     |> stateLessSync 
            --             in
            --                 syncStateRouter chekcer any stateFullNext rout req
            --                     |> shouldEqual (rout req)
            --         )
            --         , describe "Router Next"
            --             [ test "Handler Redirect" ( 
            --                 syncStateRouter chekcer str stateFullRedirect stateLessSyncNext req
            --                     |> equal (stateFulSyncResult redirect) 
            --             )
            --             , test "Handler Reply" ( 
            --                 syncStateRouter chekcer str stateFullResponse stateLessSyncNext req
            --                     |> equal (stateFulSyncResult response ) 
            --             ) 
            --             , test "Handler Next" (
            --                 syncStateRouter chekcer str stateFullNext stateLessSyncNext req
            --                     |> equal (stateFulSyncResult next )  
            --             ) 
            --             , test "Hanler URL does not match" ( 
            --                 syncStateRouter chekcer int stateFullNext stateLessSyncNext req
            --                     |> shouldEqual (stateLessSyncNext req)  
            --             )
            --             ]
            --         ]
            --     , describe "Async Router" 
            --         [ test "Router Response" (
            --             let
            --                 rout = getResponse >> Reply >> StateLess
            --             in
            --                 syncStateRouter chekcer str stateFullNext (rout >> succeed >> Async) req
            --                     |> testTask rout req
            --         )
            --         , test "Router Redirect" (
            --             let
            --                 redirect req = req.url
            --                     |> Redirect
            --                     |> StateLess
            --                 rout = redirect >> succeed >> Async
            --             in
            --                 syncStateRouter chekcer str stateFullNext rout req
            --                     |> testTask redirect req
            --         )
            --         , describe "Router Next"
            --             [ test "Handler Redirect" (
            --                 syncStateRouter chekcer str stateFullRedirect stateLessAsyncNext req
            --                     |> testTask (stateFullAsyncResult redirect ) req
            --             )
            --             , test "Handler Reply" (
            --                 syncStateRouter chekcer str stateFullResponse stateLessAsyncNext req
            --                     |> testTask (stateFullAsyncResult response ) req
            --             )
            --             , test "Handler Next" (
            --                 syncStateRouter chekcer str stateFullNext stateLessAsyncNext req
            --                     |> testTask (stateFullAsyncResult next) req  
            --             )
            --             , test "Hanler URL does not match" (
            --                 syncStateRouter chekcer int stateFullNext stateLessAsyncNext req
            --                     |> testTask stateLessNext req  
            --             )
            --             ]
            --         ]
            --     , describe "Sync State Router"
            --         [ test "Router Response" (   
            --             let
            --                 rout = 
            --                     stateFullSyncRouter response
            --             in
            --                 syncStateRouter chekcer any stateFullNext rout req
            --                     |> equal (rout req)
            --             )
            --         , test "Router Redirect" (
            --             let
            --                 rout = 
            --                     stateFullSyncRouter redirect
            --             in
            --                 syncStateRouter chekcer any stateFullNext rout req
            --                     |> equal (rout req)
            --         )
            --         , describe "Router Next"
            --             [ test "Handler Redirect" ( 
            --                 syncStateRouter chekcer str stateFullRedirect stateFullSyncNext req
            --                     |> equal (stateFullSyncResultStateFullHanlder <| stateFullRouter redirect)
            --             )
            --             , test "Handler Reply" ( 
            --                 syncStateRouter chekcer str stateFullResponse stateFullSyncNext req
            --                     |> equal (stateFullSyncResultStateFullHanlder <| stateFullRouter response)
            --             ) 
            --             , test "Handler Next" (
            --                 syncStateRouter chekcer str stateFullNext stateFullSyncNext req
            --                     |> equal (stateFullSyncResultStateFullHanlder <| stateFullRouter next)
            --             ) 
            --             , test "Hanler URL does not match" ( 
            --                 syncStateRouter chekcer int stateFullNext stateFullSyncNext req
            --                     |> equal (stateFullSyncNext req)
            --             )
            --             ]
            --         ]
            --     , describe "Async State Router"
            --         [ test "Router Response" (   
            --             let
            --                 rout = 
            --                     stateFullAsyncRouter response
            --             in
            --                 syncStateRouter chekcer any stateFullNext rout req
            --                     |> equal (rout req)
            --             )
            --         , test "Router Redirect" (
            --             let
            --                 rout = 
            --                     stateFullAsyncRouter redirect
            --             in
            --                 syncStateRouter chekcer any stateFullNext rout req
            --                     |> equal (rout req)
            --         )
            --         , describe "Router Next"
            --             [ test "Handler Redirect" ( 
            --                 syncStateRouter chekcer str stateFullRedirect stateFullAsyncNext req
            --                     |> equal (stateFullAsyncResultStateFullHanlder redirect)
            --             )
            --             , test "Handler Reply" ( 
            --                 syncStateRouter chekcer str stateFullResponse stateFullAsyncNext req
            --                     |> equal (stateFullAsyncResultStateFullHanlder response)
            --             ) 
            --             , test "Handler Next" (
            --                 syncStateRouter chekcer str stateFullNext stateFullAsyncNext req
            --                     |> equal (stateFullAsyncResultStateFullHanlder next)
            --             ) 
            --             , test "Hanler URL does not match" ( 
            --                 syncStateRouter chekcer int stateFullNext stateFullAsyncNext req
            --                     |> equal (stateFullAsyncNext req)
            --             )
            --             ]
            --         ]
            --     ]
             describe "Async StateFul Handler"
                [ describe "Sync Router" 
                    [ 
                        test "Router Response" (   
                        let
                            rout = getResponse >> Reply >> stateLessSync
                        in
                            asyncStateRouter chekcer any (stateFullNext >> succeed) rout req
                                |> shouldEqual (rout req)
                    )
                    , test "Router Redirect" (
                        let
                            rout req = req.url
                                |> Redirect
                                |> stateLessSync 
                        in
                            asyncStateRouter chekcer any (stateFullNext >> succeed) rout req
                                |> shouldEqual (rout req)
                    )
                    , describe "Router Next"
                        [ test "Handler Redirect" ( 
                            asyncStateRouter chekcer str (stateFullRedirect >> succeed) stateLessSyncNext req
                                |> equal (stateFulAsyncResult redirect) 
                        )
                        , test "Handler Reply" ( 
                            asyncStateRouter chekcer str (stateFullResponse >> succeed) stateLessSyncNext req
                                |> equal (stateFulAsyncResult response ) 
                        ) 
                        , test "Handler Next" (
                            asyncStateRouter chekcer str (stateFullNext >> succeed) stateLessSyncNext req
                                |> equal (stateFulAsyncResult next )  
                        ) 
                        , test "Hanler URL does not match" ( 
                            asyncStateRouter chekcer int (stateFullNext >> succeed) stateLessSyncNext req
                                |> shouldEqual (stateLessSyncNext req)  
                        )
                        ]
                    ]
                , describe "Async Router" 
                    [ test "Router Response" (
                        let
                            rout = getResponse >> Reply >> StateLess
                        in
                            asyncStateRouter chekcer str (stateFullNext >> succeed) (rout >> succeed >> Async) req
                                |> testTask rout req
                    )
                    , test "Router Redirect" (
                        let
                            redirect req = req.url
                                |> Redirect
                                |> StateLess
                            rout = redirect >> succeed >> Async
                        in
                            asyncStateRouter chekcer str (stateFullNext >> succeed) rout req
                                |> testTask redirect req
                    )
                    , describe "Router Next"
                        [ test "Handler Redirect" (
                            asyncStateRouter chekcer str (stateFullRedirect >> succeed) stateLessAsyncNext req
                                |> testTask (stateFullAsyncResult redirect ) req
                        )
                        , test "Handler Reply" (
                            asyncStateRouter chekcer str (stateFullResponse >> succeed) stateLessAsyncNext req
                                |> testTask (stateFullAsyncResult response ) req
                        )
                        , test "Handler Next" (
                            asyncStateRouter chekcer str (stateFullNext >> succeed) stateLessAsyncNext req
                                |> testTask (stateFullAsyncResult next) req  
                        )
                        , test "Hanler URL does not match" (
                            asyncStateRouter chekcer int (stateFullNext >> succeed) stateLessAsyncNext req
                                |> testTask stateLessNext req  
                        )
                        ]
                    ]
                -- , describe "Sync State Router"
                --     [ test "Router Response" (   
                --         let
                --             rout = 
                --                 stateFullSyncRouter response
                --         in
                --             syncStateRouter chekcer any stateFullNext rout req
                --                 |> equal (rout req)
                --         )
                --     , test "Router Redirect" (
                --         let
                --             rout = 
                --                 stateFullSyncRouter redirect
                --         in
                --             syncStateRouter chekcer any stateFullNext rout req
                --                 |> equal (rout req)
                --     )
                --     , describe "Router Next"
                --         [ test "Handler Redirect" ( 
                --             syncStateRouter chekcer str stateFullRedirect stateFullSyncNext req
                --                 |> equal (stateFullSyncResultStateFullHanlder <| stateFullRouter redirect)
                --         )
                --         , test "Handler Reply" ( 
                --             syncStateRouter chekcer str stateFullResponse stateFullSyncNext req
                --                 |> equal (stateFullSyncResultStateFullHanlder <| stateFullRouter response)
                --         ) 
                --         , test "Handler Next" (
                --             syncStateRouter chekcer str stateFullNext stateFullSyncNext req
                --                 |> equal (stateFullSyncResultStateFullHanlder <| stateFullRouter next)
                --         ) 
                --         , test "Hanler URL does not match" ( 
                --             syncStateRouter chekcer int stateFullNext stateFullSyncNext req
                --                 |> equal (stateFullSyncNext req)
                --         )
                --         ]
                --     ]
                -- , describe "Async State Router"
                --     [ test "Router Response" (   
                --         let
                --             rout = 
                --                 stateFullAsyncRouter response
                --         in
                --             syncStateRouter chekcer any stateFullNext rout req
                --                 |> equal (rout req)
                --         )
                --     , test "Router Redirect" (
                --         let
                --             rout = 
                --                 stateFullAsyncRouter redirect
                --         in
                --             syncStateRouter chekcer any stateFullNext rout req
                --                 |> equal (rout req)
                --     )
                --     , describe "Router Next"
                --         [ test "Handler Redirect" ( 
                --             syncStateRouter chekcer str stateFullRedirect stateFullAsyncNext req
                --                 |> equal (stateFullAsyncResultStateFullHanlder redirect)
                --         )
                --         , test "Handler Reply" ( 
                --             syncStateRouter chekcer str stateFullResponse stateFullAsyncNext req
                --                 |> equal (stateFullAsyncResultStateFullHanlder response)
                --         ) 
                --         , test "Handler Next" (
                --             syncStateRouter chekcer str stateFullNext stateFullAsyncNext req
                --                 |> equal (stateFullAsyncResultStateFullHanlder next)
                --         ) 
                --         , test "Hanler URL does not match" ( 
                --             syncStateRouter chekcer int stateFullNext stateFullAsyncNext req
                --                 |> equal (stateFullAsyncNext req)
                --         )
                --         ]
                --     ]
                ]
            ]

{-
-}
routerInternals : Test
routerInternals =
    describe "Router Interlan logic" (map testCheckerAndMethod methodTestCases)
