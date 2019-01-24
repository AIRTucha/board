module Board.Router.InternalsTest exposing (..)

import Ordeal exposing (Test, success, test, describe, shouldEqual, andTest, failure, all, and)
import Board.Router.Internals exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any, str, int)
import Board.Internals exposing(..)
import List exposing (concat, map)
import Task exposing (succeed, andThen)


url : String
url = "test"


response : Request a -> String -> AnswerValue a1 model error
response req str = 
    let 
        res = getResponse req
    in 
        Reply { res | content = Text "text/plain" str }


next : Request a -> String -> AnswerValue a model error
next req str =
    Next { req | content = Text "text/plain" str }


redirect : a -> String -> AnswerValue value model error
redirect req str =
    Redirect str


stateLessSyncNext : Request value -> Mode error1 (Answer value model error)
stateLessSyncNext = 
    Next >> stateLessSync


stateLessAsyncNext : Request value -> Mode error (Answer value model error1)
stateLessAsyncNext = 
    Next >> succeed >> stateLessAsync


result mismatchHandler matchMode mode req checker =
    let 
        getResult matchHandler =
            if checker req then 
                matchMode <| matchHandler req url
            else 
                mismatchHandler req
    in
        mode >> getResult
    


model : String
model =
    "model"


eqaulValues : Answer value String String -> Answer value String String -> Ordeal.Expectation
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


equal : Mode String (Answer value String String) -> Mode String (Answer value String String) -> Ordeal.Expectation
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
                        |> andTest (\ value ->
                            case value of 
                                Ok (value1, value2) ->
                                    eqaulValues value1 value2
                                
                                Err str ->
                                    failure str
                        )


toStateFullHanlder : (a -> b) -> a -> c -> ( c, b )
toStateFullHanlder handler args model =
    (model, handler args)
    

toStatelessAsync : AnswerValue value model error1 -> Mode error (Answer value model error1)
toStatelessAsync =
    succeed >> stateLessAsync


toStateFulRouter : (a -> b) -> (c -> d -> a) -> c -> d -> e -> ( e, b )
toStateFulRouter mode reqToValue req str model =
    (model, mode <| reqToValue req str )


toSyncStateHandler : (c -> d -> AnswerValue value model error) -> c -> d -> e -> ( e, Mode error1 (Answer value model error) )
toSyncStateHandler =
    toStateFulRouter stateLessSync


stateFullSyncRouter : ({ b | url : a } -> a -> AnswerValue value model error) -> { b | url : a } -> Mode error1 (Answer value model error)
stateFullSyncRouter reqToValue req = 
    stateFullSync <| toSyncStateHandler reqToValue req req.url


stateFullSyncNext : Request a -> Mode error1 (Answer a model error)
stateFullSyncNext =
    stateFullSyncRouter next


stateFullAsyncRouter : ({ b | url : a } -> a -> AnswerValue value model error) -> { b | url : a } -> Mode error1 (Answer value model error)
stateFullAsyncRouter reqToValue req = 
    stateFullAsync <| toSyncStateHandler reqToValue req req.url


stateFullAsyncNext : Request a -> Mode error1 (Answer a model error)
stateFullAsyncNext =
    stateFullAsyncRouter next


toAsyncStateHandler : (c -> d -> AnswerValue value model error1) -> c -> d -> e -> ( e, Mode x (Answer value model error1) )
toAsyncStateHandler =
    toStateFulRouter (stateLessAsync << succeed)


toAsyncStateHandlerStateFull mode handler = 
  toStateFulRouter mode (\ a s m -> (m, stateLessSync <| handler a s))


createTestDesciption description =
    let 
        reply = getResponse >> Reply
        redirectRoute mode _ = 
            url
                |> Redirect
                |> mode
    in
        { router = description.router
        , name = description.name 
        , handlersMode = description.handlersMode
        , tests = 
            [ { name = "Sync Router"
              , responseRoute = reply >> stateLessSync
              , redirectRoute = redirectRoute stateLessSync  
              , nextRoute = stateLessSyncNext
              , handlerResult = description.syncRouterResultHandler 
              }
            , { name = "Async Router"
              , responseRoute = reply >> toStatelessAsync
              , redirectRoute = redirectRoute toStatelessAsync
              , nextRoute = stateLessAsyncNext
              , handlerResult = description.asyncRouterResultHandler 
              }
            , { name = "Sync State Router"
              , responseRoute = stateFullSyncRouter response
              , redirectRoute = stateFullSyncRouter redirect
              , nextRoute = stateFullSyncNext
              , handlerResult = description.syncStateRouterResultHandler 
              }
            , { name = "Async State Router"
              , responseRoute = stateFullAsyncRouter response
              , redirectRoute = stateFullAsyncRouter redirect
              , nextRoute = stateFullAsyncNext
              , handlerResult = description.asyncStateRouterResultHandler 
              }
            ]
        }


testDescription = 
    createTestDesciption
        { router = syncRouter
        , name = "Sync StateLess Handler" 
        , handlersMode = identity
        , syncRouterResultHandler =
            result stateLessSyncNext stateLessSync identity
        , asyncRouterResultHandler = 
            result stateLessAsyncNext toStatelessAsync identity
        , syncStateRouterResultHandler =
            result stateFullSyncNext stateFullSync toSyncStateHandler
        , asyncStateRouterResultHandler =
            result stateFullAsyncNext stateFullAsync toSyncStateHandler
        }

testDescription2 =
    createTestDesciption
        { router = asyncRouter
        , name = "Async StateLess Handler" 
        , handlersMode = \ f -> f >> succeed
        , syncRouterResultHandler =
            result stateLessSyncNext toStatelessAsync identity
        , asyncRouterResultHandler =
            result stateLessAsyncNext toStatelessAsync identity
        , syncStateRouterResultHandler =
            result stateFullSyncNext stateFullSync toAsyncStateHandler
        , asyncStateRouterResultHandler = 
            result stateFullAsyncNext stateFullAsync toAsyncStateHandler
        }


testDescription3 =
    createTestDesciption
        { router = syncStateRouter
        , name = "Sync StateFul Handler" 
        , handlersMode = toStateFullHanlder
        , syncRouterResultHandler =
            result stateLessSyncNext stateFullSync toSyncStateHandler
        , asyncRouterResultHandler =
            result stateLessAsyncNext stateFullAsync toSyncStateHandler
        , syncStateRouterResultHandler = 
            result stateFullSyncNext stateFullSync <| toStateFulRouter stateLessSync >> (toStateFulRouter stateFullSync)
        , asyncStateRouterResultHandler =
            result stateFullAsyncNext stateFullAsync <| toAsyncStateHandlerStateFull stateFullSync 
        }


testDescription4 =
    let
        toAsyncStateHandlerAsyncStateFull = toAsyncStateHandlerStateFull stateFullAsync
    in
        createTestDesciption
            { router = asyncStateRouter
            , name = "Async StateFul Handler" 
            , handlersMode = \ f -> (toStateFullHanlder f) >> succeed
            , syncRouterResultHandler =
                result stateLessSyncNext stateFullAsync toSyncStateHandler
            , asyncRouterResultHandler = 
                result stateLessAsyncNext stateFullAsync toSyncStateHandler
            , syncStateRouterResultHandler =
                result stateFullSyncNext stateFullSync toAsyncStateHandlerAsyncStateFull
            , asyncStateRouterResultHandler =
                result stateFullAsyncNext stateFullAsync toAsyncStateHandlerAsyncStateFull
            }


testCheckerAndMethod {router, name, handlersMode, tests } (chekcer, method, confName) =
  let 
    req = getRequest method
    runTests req cases =
        let 
            handlerResult = cases.handlerResult req chekcer 
            getHandler hanlder (param, req) =
                case param of
                    StrParam str ->
                        hanlder req str
                    
                    _ -> 
                        Next req
            toNext = 
                handlersMode <| getHandler next
        in
            describe cases.name
                [ test "Router Response" (   
                    router chekcer any toNext (cases.responseRoute) req
                        |> equal (cases.responseRoute req)
                )
                , test "Router Redirect" (
                    router chekcer any toNext (cases.redirectRoute) req
                        |> equal (cases.redirectRoute req)
                )
                , describe "Router Next"
                    [ test "Handler Redirect" ( 
                        router chekcer str (handlersMode <| getHandler redirect) cases.nextRoute req
                            |> equal (handlerResult redirect )
                    )
                    , test "Handler Reply" ( 
                        router chekcer str (handlersMode <| getHandler response) cases.nextRoute req
                            |> equal (handlerResult response ) 
                    ) 
                    , test "Handler Next" (
                        router chekcer str toNext cases.nextRoute req
                            |> equal (handlerResult next )  
                    ) 
                    , test "Hanler URL does not match" ( 
                        router chekcer int toNext cases.nextRoute req
                            |> equal (cases.nextRoute req)  
                    )
                    ]
                ]
  in
    describe ( name ++ ": " ++ confName ) (
      map ( runTests { req | url = url } ) tests
    )
        

{-
-}
routerInternals : Test
routerInternals =
    let 
        jounMethodToChecker (checker, checkerName) (method, methodName) =
            (checker, method, "Expects " ++ checkerName ++ ", received " ++ methodName)
        joinCheckerToMethods checkerTuple =
            [ ( Get, "Get" )
            , ( Post, "Post" )
            , ( Put, "Put" )
            , ( Delete, "Delete" )
            ]
                |> map (jounMethodToChecker checkerTuple)
        methodTestCases =
            [ ( isGet, "Get" )
            , ( isPost, "Post" )
            , ( isPut, "Put" )
            , ( isDelete, "Delete" )
            , ( anyMethod, "Any" )
            ] 
                |> map joinCheckerToMethods
                |> concat
    in
        describe "Router Interlan logic" <|
            (map (testCheckerAndMethod testDescription) methodTestCases) ++
            (map (testCheckerAndMethod testDescription2) methodTestCases) ++
            (map (testCheckerAndMethod testDescription3) methodTestCases) ++ 
            (map (testCheckerAndMethod testDescription4) methodTestCases)
