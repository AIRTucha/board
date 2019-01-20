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


testTask resultValue result = 
    let 
        checkResult result = 
            case result of 
                Ok value ->
                    eqaulValues value resultValue

                Err str ->
                    failure str
    in
        result
            |> liftToAsync
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


toStateFullHanlder handler args model =
    (model, handler args)


stateFullNext = 
    toStateFullHanlder toNext


stateFullRedirect =
    toStateFullHanlder toRedirect


stateFullResponse =
    toStateFullHanlder toResponse


-- testCheckerAndMethod (chekcer, method, name) =
--     let 
--         req = request method
--         stateLessSyncResult = 
--             result chekcer req stateLessSyncNext stateLessSync
--         stateFulSyncResult = 
--             toSyncStateHandler >> result chekcer req stateLessSyncNext stateFullSync 
--         stateFulAsyncResult = 
--             toSyncStateHandler >> result chekcer req stateLessSyncNext stateFullAsync
--         stateLessAsyncResult handler _ = 
--             result chekcer req stateLessNext StateLess handler
--         stateFullAsyncResult handler _ = 
--             result chekcer req stateLessNext StateFull (toSyncStateHandler handler)
--         toStateFulRouter mode reqToValue req str model =
--            (model, mode <| reqToValue req str )
--         toSyncStateHandler =
--            toStateFulRouter stateLessSync
--         toSyncStateHandlerStateFull = 
--            toStateFulRouter stateFullSync
--         toAsyncStateHandler =
--            toStateFulRouter (stateLessAsync << succeed)
--         stateFullSyncRouter reqToValue req = 
--             stateFullSync <| toSyncStateHandler reqToValue req req.url
--         stateFullSyncNext =
--             stateFullSyncRouter next
--         stateFullSyncResult reqToValue =
--             result chekcer req stateFullSyncNext stateFullSync <| toSyncStateHandler reqToValue
--         stateFullSyncResultStateFullHanlder reqToValue =
--             result chekcer req stateFullSyncNext stateFullSync <| toSyncStateHandlerStateFull reqToValue
--         stateFullAsyncResultStateFullHanlder reqToValue =
--             result chekcer req stateFullAsyncNext stateFullAsync <| (toStateFulRouter stateFullSync (\ a s m -> (m, stateLessSync <| reqToValue a s))) 
--         stateFullAsyncResultStateFullAsyncHanlder reqToValue =
--             result chekcer req stateFullSyncNext stateFullSync <| (toStateFulRouter stateFullAsync (\ a s m -> (m, stateLessSync <| reqToValue a s))) 
--         stateFullAsyncResultStateFullAsyncHanlderAsync reqToValue =
--             result chekcer req stateFullAsyncNext stateFullAsync <| (toStateFulRouter stateFullAsync (\ a s m -> (m, stateLessSync <| reqToValue a s))) 
--         stateFullAsyncRouter reqToValue req = 
--             stateFullAsync <| toSyncStateHandler reqToValue req req.url
--         stateFullAsyncNext =
--             stateFullAsyncRouter next
--         asyncStateFullAsyncResultSync reqToValue =
--             result chekcer req stateFullAsyncNext stateFullAsync <| toSyncStateHandler reqToValue
--         syncStateFullSyncResultAsync reqToValue =
--             result chekcer req stateFullSyncNext stateFullSync <| toAsyncStateHandler reqToValue
--         asyncStateFullAsyncResultAsync reqToValue =
--             result chekcer req stateFullAsyncNext stateFullAsync <| toAsyncStateHandler reqToValue
--     in
--         describe name
--             [ 
--             , describe "Async StateFul Handler"
--                 [ describe "Sync Router" 
--                     [ 
--                         test "Router Response" (   
--                         let
--                             rout = getResponse >> Reply >> stateLessSync
--                         in
--                             asyncStateRouter chekcer any (stateFullNext >> succeed) rout req
--                                 |> equal (rout req)
--                     )
--                     , test "Router Redirect" (
--                         let
--                             rout req = req.url
--                                 |> Redirect
--                                 |> stateLessSync 
--                         in
--                             asyncStateRouter chekcer any (stateFullNext >> succeed) rout req
--                                 |> equal (rout req)
--                     )
--                     , describe "Router Next"
--                         [ test "Handler Redirect" ( 
--                             asyncStateRouter chekcer str (stateFullRedirect >> succeed) stateLessSyncNext req
--                                 |> equal (stateFulAsyncResult redirect) 
--                         )
--                         , test "Handler Reply" ( 
--                             asyncStateRouter chekcer str (stateFullResponse >> succeed) stateLessSyncNext req
--                                 |> equal (stateFulAsyncResult response ) 
--                         ) 
--                         , test "Handler Next" (
--                             asyncStateRouter chekcer str (stateFullNext >> succeed) stateLessSyncNext req
--                                 |> equal (stateFulAsyncResult next )  
--                         ) 
--                         , test "Hanler URL does not match" ( 
--                             asyncStateRouter chekcer int (stateFullNext >> succeed) stateLessSyncNext req
--                                 |> equal (stateLessSyncNext req)  
--                         )
--                         ]
--                     ]
--                 , describe "Async Router" 
--                     [ test "Router Response" (
--                         let
--                             rout = getResponse >> Reply >> StateLess
--                         in
--                             asyncStateRouter chekcer str (stateFullNext >> succeed) (rout >> succeed >> Async) req
--                                 |> testTask rout req
--                     )
--                     , test "Router Redirect" (
--                         let
--                             redirect req = req.url
--                                 |> Redirect
--                                 |> StateLess
--                             rout = redirect >> succeed >> Async
--                         in
--                             asyncStateRouter chekcer str (stateFullNext >> succeed) rout req
--                                 |> testTask redirect req
--                     )
--                     , describe "Router Next"
--                         [ test "Handler Redirect" (
--                             asyncStateRouter chekcer str (stateFullRedirect >> succeed) stateLessAsyncNext req
--                                 |> testTask (stateFullAsyncResult redirect ) req
--                         )
--                         , test "Handler Reply" (
--                             asyncStateRouter chekcer str (stateFullResponse >> succeed) stateLessAsyncNext req
--                                 |> testTask (stateFullAsyncResult response ) req
--                         )
--                         , test "Handler Next" (
--                             asyncStateRouter chekcer str (stateFullNext >> succeed) stateLessAsyncNext req
--                                 |> testTask (stateFullAsyncResult next) req  
--                         )
--                         , test "Hanler URL does not match" (
--                             asyncStateRouter chekcer int (stateFullNext >> succeed) stateLessAsyncNext req
--                                 |> testTask stateLessNext req  
--                         )
--                         ]
--                     ]
--                 , describe "Sync State Router"
--                     [
--                          test "Router Response" (   
--                         let
--                             rout = 
--                                 stateFullSyncRouter response
--                         in
--                             asyncStateRouter chekcer any (stateFullNext >> succeed) rout req
--                                 |> equal (rout req)
--                         )
--                     , test "Router Redirect" (
--                         let
--                             rout = 
--                                 stateFullSyncRouter redirect
--                         in
--                             asyncStateRouter chekcer any (stateFullNext >> succeed) rout req
--                                 |> equal (rout req)
--                     )
--                     , describe "Router Next"
--                         [ test "Handler Redirect" ( 
--                             asyncStateRouter chekcer str (stateFullRedirect >> succeed) stateFullSyncNext req
--                                 |> equal ( stateFullAsyncResultStateFullAsyncHanlder redirect )
--                         )
--                         , test "Handler Reply" ( 
--                             asyncStateRouter chekcer str (stateFullResponse >> succeed) stateFullSyncNext req
--                                 |> equal (stateFullAsyncResultStateFullAsyncHanlder response)
--                         ) 
--                         , test "Handler Next" (
--                             asyncStateRouter chekcer str (stateFullNext >> succeed) stateFullSyncNext req
--                                 |> equal (stateFullAsyncResultStateFullAsyncHanlder next)
--                         ) 
--                         , test "Hanler URL does not match" ( 
--                             asyncStateRouter chekcer int (stateFullNext >> succeed) stateFullSyncNext req
--                                 |> equal (stateFullSyncNext req)
--                         )
--                         ]
--                     ]
--                 , describe "Async State Router"
--                     [ test "Router Response" (   
--                         let
--                             rout = 
--                                 stateFullAsyncRouter response
--                         in
--                             asyncStateRouter chekcer any (stateFullNext >> succeed) rout req
--                                 |> equal (rout req)
--                         )
--                     , test "Router Redirect" (
--                         let
--                             rout = 
--                                 stateFullAsyncRouter redirect
--                         in
--                             asyncStateRouter chekcer any (stateFullNext >> succeed) rout req
--                                 |> equal (rout req)
--                     )
--                     , describe "Router Next"
--                         [ test "Handler Redirect" ( 
--                             asyncStateRouter chekcer str (stateFullRedirect >> succeed) stateFullAsyncNext req
--                                 |> equal (stateFullAsyncResultStateFullAsyncHanlderAsync redirect)
--                         )
--                         , test "Handler Reply" ( 
--                             asyncStateRouter chekcer str (stateFullResponse >> succeed) stateFullAsyncNext req
--                                 |> equal (stateFullAsyncResultStateFullAsyncHanlderAsync response)
--                         ) 
--                         , test "Handler Next" (
--                             asyncStateRouter chekcer str (stateFullNext >> succeed) stateFullAsyncNext req
--                                 |> equal (stateFullAsyncResultStateFullAsyncHanlderAsync next)
--                         ) 
--                         , test "Hanler URL does not match" ( 
--                             asyncStateRouter chekcer int (stateFullNext >> succeed) stateFullAsyncNext req
--                                 |> equal (stateFullAsyncNext req)
--                         )
--                         ]
--                     ]
--                 ]
--             ]

runTests router chekcer method req cases =
    let 
        handlerResult = cases.handlerResult req chekcer 
    in
        describe cases.name
            [ 
              test "Router Response" (   
                router chekcer any cases.toNext (cases.responseRoute >> cases.routeMode) req
                    |> equal (cases.responseRoute req)
            )
            , test "Router Redirect" (
                router chekcer any cases.toNext (cases.redirectRoute>> cases.routeMode) req
                    |> equal (cases.redirectRoute req)
            )
            , describe "Router Next"
                [ test "Handler Redirect" ( 
                    router chekcer str cases.toRedirect cases.nextRoute req
                        |> equal (handlerResult redirect )
                )
                , test "Handler Reply" ( 
                    router chekcer str cases.toResponse cases.nextRoute req
                        |> equal (handlerResult response ) 
                ) 
                , test "Handler Next" (
                    router chekcer str cases.toNext cases.nextRoute req
                        |> equal (handlerResult next )  
                ) 
                , test "Hanler URL does not match" ( 
                    router chekcer int cases.toNext cases.nextRoute req
                        |> equal (cases.nextRoute req)  
                )
                ]
            ]

testSubCases runTests chekcer method req subCases = 
  [
    describe subCases.name (
        subCases.tests
            |> map (runTests subCases.router chekcer method req)
    )
    
    ]

testCheckerAndMethod2 cases (chekcer, method, name) =
    let 
        req = request method
    in
        describe name (
            cases 
                |> (testSubCases runTests chekcer method req)
            )
            

toStatelessAsync =
    succeed >> stateLessAsync

toStateFulRouter mode reqToValue req str model =
    (model, mode <| reqToValue req str )

toSyncStateHandler =
    toStateFulRouter stateLessSync

stateFullSyncRouter reqToValue req = 
    stateFullSync <| toSyncStateHandler reqToValue req req.url

stateFullSyncNext =
    stateFullSyncRouter next

stateFullAsyncRouter reqToValue req = 
    stateFullAsync <| toSyncStateHandler reqToValue req req.url

stateFullAsyncNext =
    stateFullAsyncRouter next

toAsyncStateHandler =
    toStateFulRouter (stateLessAsync << succeed)

toSyncStateHandlerStateFull = 
    toStateFulRouter stateFullSync

testDescription = 
    { router = syncRouter
        , name = "Sync StateLess Handler" 
        , tests = 
            [ { name = "Sync Router"
              , toNext = toNext
              , toRedirect = toRedirect
              , toResponse = toResponse
              , responseRoute = getResponse >> Reply >> stateLessSync
              , redirectRoute = \_ -> "test"
                  |> Redirect
                  |> stateLessSync  
              , routeMode = identity
              , nextRoute = stateLessSyncNext
              , handlerResult = \ req chekcer handler ->
                      result chekcer req stateLessSyncNext stateLessSync handler
              }
            , { name = "Async Router"
              , toNext = toNext
              , toRedirect = toRedirect
              , toResponse = toResponse
              , responseRoute = getResponse >> Reply >> toStatelessAsync
              , redirectRoute = \ _ -> "test"
                  |> Redirect
                  |> toStatelessAsync
              , routeMode = identity
              , nextRoute = stateLessAsyncNext
              , handlerResult = \ req chekcer handler ->
                      result chekcer req stateLessAsyncNext toStatelessAsync handler
            }
            , { name = "Sync State Router"
              , toNext = toNext
              , toRedirect = toRedirect
              , toResponse = toResponse
              , responseRoute = stateFullSyncRouter response
              , redirectRoute = stateFullSyncRouter redirect
              , routeMode = identity
              , nextRoute = stateFullSyncNext
              , handlerResult = \ req chekcer handler ->
                      result chekcer req stateFullSyncNext stateFullSync (toSyncStateHandler handler)
              }
            , { name = "Async State Router"
              , toNext = toNext
              , toRedirect = toRedirect
              , toResponse = toResponse
              , responseRoute = stateFullAsyncRouter response
              , redirectRoute = stateFullAsyncRouter redirect
              , routeMode = identity
              , nextRoute = stateFullAsyncNext
              , handlerResult = \ req chekcer handler ->
                      result chekcer req stateFullAsyncNext stateFullAsync (toSyncStateHandler handler)
              }
            ]
        }

testDescription2 =
        { router = asyncRouter
        , name = "Async StateLess Handler" 
        , tests = 
            [ { name = "Sync Router"
              , toNext = toNextTask
              , toRedirect = toRedirectTask
              , toResponse = toResponseTask
              , responseRoute = getResponse >> Reply >> stateLessSync
              , redirectRoute = \_ -> "test"
                  |> Redirect
                  |> stateLessSync  
              , routeMode = identity
              , nextRoute = stateLessSyncNext
              , handlerResult = \ req chekcer handler ->
                  result chekcer req stateLessSyncNext toStatelessAsync handler
              }
            , { name = "Async Router"
              , toNext = toNextTask
              , toRedirect = toRedirectTask
              , toResponse = toResponseTask
              , responseRoute = getResponse >> Reply >> toStatelessAsync
              , redirectRoute = \ _ -> "test"
                  |> Redirect
                  |> toStatelessAsync
              , routeMode = identity
              , nextRoute = stateLessAsyncNext
              , handlerResult = \ req chekcer handler ->
                      result chekcer req stateLessAsyncNext toStatelessAsync handler
              }
            , { name = "Sync State Router"
              , toNext = toNextTask
              , toRedirect = toRedirectTask
              , toResponse = toResponseTask
              , responseRoute = stateFullSyncRouter response
              , redirectRoute = stateFullSyncRouter redirect
              , routeMode = identity
              , nextRoute = stateFullSyncNext
              , handlerResult = \ req chekcer handler ->
                  result chekcer req stateFullSyncNext stateFullSync <| toAsyncStateHandler handler
              }
            , { name = "Async State Router"
              , toNext = toNextTask
              , toRedirect = toRedirectTask
              , toResponse = toResponseTask
              , responseRoute = stateFullAsyncRouter response
              , redirectRoute = stateFullAsyncRouter redirect
              , routeMode = identity
              , nextRoute = stateFullAsyncNext
              , handlerResult = \ req chekcer handler ->
                result chekcer req stateFullAsyncNext stateFullAsync <| toAsyncStateHandler handler
              }
            ]
        }


testDescription3 =
        { router = syncStateRouter
        , name = "Sync StateFul Handler" 
        , tests = 
            [ { name = "Sync Router"
              , toNext = stateFullNext
              , toRedirect = stateFullRedirect
              , toResponse = stateFullResponse
              , responseRoute = getResponse >> Reply >> stateLessSync
              , redirectRoute = \_ -> "test"
                  |> Redirect
                  |> stateLessSync  
              , routeMode = identity
              , nextRoute = stateLessSyncNext
              , handlerResult = \ req chekcer handler ->
                  (toSyncStateHandler >> result chekcer req stateLessSyncNext stateFullSync) handler
              }
            , { name = "Async Router"
              , toNext = stateFullNext
              , toRedirect = stateFullRedirect
              , toResponse = stateFullResponse
              , responseRoute = getResponse >> Reply >> toStatelessAsync
              , redirectRoute = \ _ -> "test"
                  |> Redirect
                  |> toStatelessAsync
              , routeMode = identity
              , nextRoute = stateLessAsyncNext
              , handlerResult = \ req chekcer handler ->
                  (toSyncStateHandler >> result chekcer req stateLessAsyncNext stateFullAsync) handler
              }
            , { name = "Sync State Router"
              , toNext = stateFullNext
              , toRedirect = stateFullRedirect
              , toResponse = stateFullResponse
              , responseRoute = stateFullSyncRouter response
              , redirectRoute = stateFullSyncRouter redirect
              , routeMode = identity
              , nextRoute = stateFullSyncNext
              , handlerResult = \ req chekcer handler ->
                  result chekcer req stateFullSyncNext stateFullSync <| toSyncStateHandlerStateFull <| stateFullRouter handler
              }
            , { name = "Async State Router"
              , toNext = stateFullNext
              , toRedirect = stateFullRedirect
              , toResponse = stateFullResponse
              , responseRoute = stateFullAsyncRouter response
              , redirectRoute = stateFullAsyncRouter redirect
              , routeMode = identity
              , nextRoute = stateFullAsyncNext
              , handlerResult = \ req chekcer handler ->
                  result chekcer req stateFullAsyncNext stateFullAsync <| (toStateFulRouter stateFullSync (\ a s m -> (m, stateLessSync <| handler a s))) 
              }
            ]
        }


{-
-}
routerInternals : Test
routerInternals =
    describe "Router Interlan logic" <|
      -- (map (testCheckerAndMethod2 testDescription) methodTestCases) ++
      -- (map (testCheckerAndMethod2 testDescription2) methodTestCases) ++
      (map (testCheckerAndMethod2 testDescription3) methodTestCases)
