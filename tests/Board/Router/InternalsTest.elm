module Board.Router.InternalsTest exposing (..)

import Ordeal exposing (Test, success, test, describe, shouldEqual, andTest, failure, all, and)
import Board.Router.Internals exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any, str, int)
import Board.Internals exposing(..)
import List exposing (concat, map)
import Task exposing (succeed, andThen)


jounMethodToChecker : ( a, String ) -> ( b, String ) -> ( a, b, String )
jounMethodToChecker (checker, checkerName) (method, methodName) =
  (checker, method, "Expects " ++ checkerName ++ ", received " ++ methodName)


joinCheckerToMethods : ( a, String ) -> List ( a, Method, String )
joinCheckerToMethods checkerTuple =
  [ ( Get, "Get" )
  , ( Post, "Post" )
  , ( Put, "Put" )
  , ( Delete, "Delete" )
  ]
    |> map (jounMethodToChecker checkerTuple)


methodTestCases : List ( Request a -> Bool, Method, String )
methodTestCases =
  [ ( isGet, "Get" )
  , ( isPost, "Post" )
  , ( isPut, "Put" )
  , ( isDelete, "Delete" )
  , ( anyMethod, "Any" )
  ] 
    |> map joinCheckerToMethods
    |> concat


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


getHandler : (Request value -> String -> AnswerValue value model error) -> ( Params, Request value ) -> AnswerValue value model error
getHandler hanlder (param, req) =
    case param of
        StrParam str ->
            hanlder req str
        
        _ -> 
            Next req


toResponse : ( Params, Request a1 ) -> AnswerValue a1 model error
toResponse =
    getHandler response


toNext : ( Params, Request a ) -> AnswerValue a model error
toNext = 
    getHandler next


toRedirect : ( Params, Request value ) -> AnswerValue value model error
toRedirect =
    getHandler redirect


stateLessSyncNext : Request value -> Mode error1 (Answer value model error)
stateLessSyncNext = 
    Next >> stateLessSync


stateLessAsyncNext : Request value -> Mode error (Answer value model error1)
stateLessAsyncNext = 
    Next >> succeed >> stateLessAsync


result : (a -> Bool) -> a -> (a -> b) -> (c -> b) -> (a -> String -> c) -> b
result checker req mismatchHandler matchMode matchHandler =
    if checker req then 
        matchMode <| matchHandler req url
    else 
        mismatchHandler req


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
                        |> andTest (\ result ->
                            case result of 
                                Ok (value1, value2) ->
                                    eqaulValues value1 value2
                                
                                Err str ->
                                    failure str
                        )


toStateFullHanlder : (a -> b) -> a -> c -> ( c, b )
toStateFullHanlder handler args model =
    (model, handler args)


stateFullNext : ( Params, Request a ) -> c -> ( c, AnswerValue a model error )
stateFullNext = 
    toStateFullHanlder toNext


stateFullRedirect : ( Params, Request value ) -> c -> ( c, AnswerValue value model error )
stateFullRedirect =
    toStateFullHanlder toRedirect


stateFullResponse : ( Params, Request a1 ) -> c -> ( c, AnswerValue a1 model error )
stateFullResponse =
    toStateFullHanlder toResponse


runTests : ( b -> Pathfinder.URL -> c -> (d -> Mode String (Answer value String String)) -> d -> Mode String (Answer value String String) ) -> b -> e -> d -> { f | toRedirect : c, toResponse : c, toNext : c } -> { g | handlerResult : d -> b -> (Request a -> String -> AnswerValue a model error) -> Mode String (Answer value String String) , name : String , redirectRoute : d -> Mode String (Answer value String String) , responseRoute : d -> Mode String (Answer value String String) , nextRoute : d -> Mode String (Answer value String String) } -> Test
runTests router chekcer method req handlers cases =
    let 
        handlerResult = cases.handlerResult req chekcer 
    in
        describe cases.name
            [ test "Router Response" (   
                router chekcer any handlers.toNext (cases.responseRoute) req
                    |> equal (cases.responseRoute req)
              )
            , test "Router Redirect" (
                router chekcer any handlers.toNext (cases.redirectRoute) req
                    |> equal (cases.redirectRoute req)
              )
            , describe "Router Next"
                [ test "Handler Redirect" ( 
                    router chekcer str handlers.toRedirect cases.nextRoute req
                        |> equal (handlerResult redirect )
                  )
                , test "Handler Reply" ( 
                    router chekcer str handlers.toResponse cases.nextRoute req
                        |> equal (handlerResult response ) 
                  ) 
                , test "Handler Next" (
                    router chekcer str handlers.toNext cases.nextRoute req
                        |> equal (handlerResult next )  
                  ) 
                , test "Hanler URL does not match" ( 
                    router chekcer int handlers.toNext cases.nextRoute req
                        |> equal (cases.nextRoute req)  
                  )
                ]
            ]


testCheckerAndMethod : { g1 | handlers : { f | toNext : c, toRedirect : c, toResponse : c } , name : String , router : b -> Pathfinder.URL -> c -> (Request a -> Mode String (Answer value String String)) -> Request a -> Mode String (Answer value String String) , tests : List { g | handlerResult : Request a -> b -> ( Request a1 -> String -> AnswerValue a1 model error ) -> Mode String (Answer value String String) , name : String , nextRoute : Request a -> Mode String (Answer value String String) , redirectRoute : Request a -> Mode String (Answer value String String) , responseRoute : Request a -> Mode String (Answer value String String) } } -> ( b, Method, String ) -> Test
testCheckerAndMethod cases (chekcer, method, name) =
  let 
    req = getRequest method
  in
    describe ( cases.name ++ ": " ++ name ) (
      map (runTests cases.router chekcer method { req | url = url } cases.handlers) cases.tests
    )
            

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


toSyncStateHandlerStateFull : (c -> d -> StateHandler value model error) -> c -> d -> e -> ( e, Mode error1 (Answer value model error) )
toSyncStateHandlerStateFull = 
    toStateFulRouter stateFullSync



toAsyncStateHandlerStateFull mode handler = 
  toStateFulRouter mode (\ a s m -> (m, stateLessSync <| handler a s))

createSyncRouter handlerResult = 
  { name = "Sync Router"
  , responseRoute = getResponse >> Reply >> stateLessSync
  , redirectRoute = \_ -> url
      |> Redirect
      |> stateLessSync  
  , nextRoute = stateLessSyncNext
  , handlerResult = handlerResult 
  }

createAsyncRouter handlerResult =
  { name = "Async Router"
  , responseRoute = getResponse >> Reply >> toStatelessAsync
  , redirectRoute = \ _ -> url
      |> Redirect
      |> toStatelessAsync
  , nextRoute = stateLessAsyncNext
  , handlerResult = handlerResult
  }

createSyncStateRouter handlerResult = 
  { name = "Sync State Router"
  , responseRoute = stateFullSyncRouter response
  , redirectRoute = stateFullSyncRouter redirect
  , nextRoute = stateFullSyncNext
  , handlerResult = handlerResult
  }

createAsyncStateRouter handlerResult = 
  { name = "Async State Router"
  , responseRoute = stateFullAsyncRouter response
  , redirectRoute = stateFullAsyncRouter redirect
  , nextRoute = stateFullAsyncNext
  , handlerResult = handlerResult
  }
testDescription = 
    { router = syncRouter
        , name = "Sync StateLess Handler" 
        , handlers = 
          { toNext = toNext
          , toRedirect = toRedirect
          , toResponse = toResponse
          } 
        , tests = 
            [ createSyncRouter <| \ req chekcer -> 
                result chekcer req stateLessSyncNext stateLessSync 
            , createAsyncRouter <| \ req chekcer ->
                result chekcer req stateLessAsyncNext toStatelessAsync
            , createSyncStateRouter <| \ req chekcer ->
                  toSyncStateHandler >> result chekcer req stateFullSyncNext stateFullSync
            , createAsyncStateRouter <| \ req chekcer ->
                  toSyncStateHandler >> result chekcer req stateFullAsyncNext stateFullAsync
            ]
        }

testDescription2 =
        { router = asyncRouter
        , name = "Async StateLess Handler" 
        , handlers = 
          { toNext = toNext >> succeed
          , toRedirect = toRedirect >> succeed
          , toResponse = toResponse >> succeed
          }
        , tests = 
            [ createSyncRouter <| \ req chekcer ->
                result chekcer req stateLessSyncNext toStatelessAsync
            , createAsyncRouter <| \ req chekcer ->
                result chekcer req stateLessAsyncNext toStatelessAsync
            , createSyncStateRouter <| \ req chekcer ->
                toAsyncStateHandler >> result chekcer req stateFullSyncNext stateFullSync
            , createAsyncStateRouter <| \ req chekcer ->
                  toAsyncStateHandler >> result chekcer req stateFullAsyncNext stateFullAsync
            ]
        }


testDescription3 =
        { router = syncStateRouter
        , name = "Sync StateFul Handler" 
        , handlers = 
          { toNext = stateFullNext
          , toRedirect = stateFullRedirect
          , toResponse = stateFullResponse
          }
        , tests = 
            [ createSyncRouter <| \ req chekcer ->
                toSyncStateHandler >> result chekcer req stateLessSyncNext stateFullSync
            , createAsyncRouter <| \ req chekcer ->
                  toSyncStateHandler >> result chekcer req stateLessAsyncNext stateFullAsync
            , createSyncStateRouter <| \ req chekcer ->
                  toStateFulRouter stateLessSync >> toSyncStateHandlerStateFull >> result chekcer req stateFullSyncNext stateFullSync 
            , createAsyncStateRouter <| \ req chekcer ->
                  toAsyncStateHandlerStateFull stateFullSync >> result chekcer req stateFullAsyncNext stateFullAsync
            ]
        }


testDescription4 =
  let
    toAsyncStateHandlerAsyncStateFull = toAsyncStateHandlerStateFull stateFullAsync
  in
    { router = asyncStateRouter
    , name = "Async StateFul Handler" 
    , handlers = 
      { toNext = stateFullNext >> succeed
      , toRedirect = stateFullRedirect >> succeed
      , toResponse = stateFullResponse >> succeed
      }
    , tests = 
        [ createSyncRouter <| \ req chekcer ->
            toSyncStateHandler >> result chekcer req stateLessSyncNext stateFullAsync
        , createAsyncRouter <| \ req chekcer ->
            toSyncStateHandler >> result chekcer req stateLessAsyncNext stateFullAsync
        , createSyncStateRouter <| \ req chekcer ->
              toAsyncStateHandlerAsyncStateFull >> result chekcer req stateFullSyncNext stateFullSync 
        , createAsyncStateRouter <| \ req chekcer ->
              toAsyncStateHandlerAsyncStateFull >> result chekcer req stateFullAsyncNext stateFullAsync
        ]
    }


{-
-}
routerInternals : Test
routerInternals =
    describe "Router Interlan logic" <|
      (map (testCheckerAndMethod testDescription) methodTestCases) ++
      (map (testCheckerAndMethod testDescription2) methodTestCases) ++
      (map (testCheckerAndMethod testDescription3) methodTestCases) ++ 
      (map (testCheckerAndMethod testDescription4) methodTestCases)
