module Board.Router.Internals exposing (..)

{-| Utility functionsandtypes for 
@docs applyState
    , toStateFullAsync
    , stateLessSync
    , nextStateLessSync
    , toStateFull
    , stateFullSync
    , stateFullAsync
    , toStateFullSync
    , stateLessAsync
    , router
    , processModeAnswer
    , handleState
    , processAsyncAnswer
    , tryToProcessRequest
    , syncRouter
    , asyncRouter
    , syncStateRouter
    , asyncStateRouter
-}
import Pathfinder exposing (..)
import Result
import Task exposing (Task, succeed, map, andThen)
import Basics exposing (..)
import Board.Router.Param exposing (parsingResult2params)
import Board.Internals exposing (..)
import Board.Shared exposing (..)


{-| Apply model to state handler and tag answer
-}
applyState : (a -> b) -> (c -> ( d, a )) -> c -> ( d, b )
applyState mode handler model =
    let 
        (newModel, answer) = handler model   
    in
        (newModel, mode answer)
    

{-| Pack Task of state handler function into Mode or Answer
-}
toStateFullAsync 
    : Task error (model -> ( model, AnswerValue value model error1 )) 
    -> Mode error (Answer value model error1)
toStateFullAsync stateHandler =
    stateHandler
        |> map toStateFull
        |> Async

{-| Turn answer value to Mode of Answer
-}
stateLessSync 
    : AnswerValue value model error 
    -> Mode error1 (Answer value model error)
stateLessSync = 
    Sync << StateLess


{-| Turns Request to Mode of stateless sync Answer
-}
nextStateLessSync : Request value -> Mode error1 (Answer value model error)
nextStateLessSync =
    stateLessSync << Next


{-| Turns uncomplited state handler to stateful Answer
-}
toStateFull : IncompliteStateHandler value model error -> Answer value model error
toStateFull =
    StateFull << ( applyState <| Sync << StateLess )


{-| Turns state handler to Sync Mode
-}
stateFullSync : StateHandler value model error -> Mode error1 (Answer value model error)
stateFullSync =
    Sync << StateFull


{-| Turns state handler to Async Mode
-}
stateFullAsync : StateHandler value model error -> Mode error1 (Answer value model error)
stateFullAsync =
    Async << succeed << StateFull


{-| Turns state handler to Async Mode
-}
toStateFullSync : IncompliteStateHandler value model error -> Mode error1 (Answer value model error)
toStateFullSync =
    Sync << toStateFull


{-| Turns Task of raw answer to async answer
-}
stateLessAsync : Task error (AnswerValue value model error1) -> Mode error (Answer value model error1)
stateLessAsync = 
    Async << map StateLess
        


{-| The most general router combinator function which is used to produce more ad-hoc solutions
-}
router 
    : ModePacker answer value model error
    -> RequestChecker value
    -> URL 
    -> PathHandler value answer 
    -> Router error value model 
    -> Router error value model 
router mode checkMethod url handler priorRouter request =
    request
        |> priorRouter 
        |> processModeAnswer mode checkMethod url handler


{-| Process Mode Answer of prior router and 
pass response down to current handler if it is needed
-}
processModeAnswer 
    : ModePacker answer value model error 
    -> RequestChecker value
    -> URL 
    -> PathHandler value answer 
    -> Mode error (Answer value model error) 
    -> Mode error (Answer value model error)
processModeAnswer mode checkMethod url handler modeAnswer =
    case modeAnswer of    
        Sync answer ->
            case answer of
                StateLess value ->
                    case value of 
                        Next newRequest ->
                            tryToProcessRequest mode checkMethod handler url newRequest
                        
                        _ ->
                            modeAnswer

                StateFull stateHandler ->
                    stateHandler
                        |> handleState mode checkMethod handler url 
                        |> stateFullSync

        Async taskAnswer ->
            taskAnswer 
                |> andThen (processAsyncAnswer mode checkMethod handler url)
                |> Async


{-| Apply state, handle and procceed to following router
-}
handleState 
    : ModePacker answer value model error
    -> RequestChecker value 
    -> PathHandler value answer
    -> URL 
    -> StateHandler value model error
    -> StateHandler value model error
handleState mode checkMethod handler url stateHandler model =
    let
        (newModel, answer) = stateHandler model
    in
        ( newModel
        , processModeAnswer mode checkMethod url handler answer
        )


{-| Handle asynchronius reply in one of three ways:

- Lift next route handler to async
- Return final answer
- Apply state
-}
processAsyncAnswer 
    :  ModePacker answer value model error
    -> RequestChecker value 
    -> PathHandler value answer
    -> URL 
    -> Answer value model error
    -> Task error (Answer value model error)
processAsyncAnswer mode checkMethod handler url answer =
    case answer of
        StateLess value ->
            case value of 
                Next request ->
                    request
                        |> tryToProcessRequest mode checkMethod handler url
                        |> liftToAsync
                
                _ ->
                    succeed answer

        StateFull stateHandler ->
            stateHandler
                |> handleState mode checkMethod handler url 
                |> StateFull
                |> succeed


{-| Handles incoming request:

- Verifies request HTTP method
- Parse URI with Pathfinder
- Pass params to route handler
-}
tryToProcessRequest 
    : ModePacker answer value model error 
    -> RequestChecker value
    -> PathHandler value answer 
    -> URL 
    -> Request value
    -> Mode error (Answer value model error)
tryToProcessRequest mode checkMethod handler url request =
    if checkMethod request then
        case parse url request.url of
            Failure _ ->
                nextStateLessSync request
            
            value ->
                case parsingResult2params value of
                    Ok params ->
                        (params, request)
                            |> handler
                            |> mode
                    
                    Err _ -> 
                        nextStateLessSync request

    else
        nextStateLessSync request


{-| Router combinator for synchronous handling of specified rout
-}
syncRouter 
    : RequestChecker value 
    -> URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error value model 
    -> Router error value model
syncRouter = 
    router stateLessSync


{-| Router combinator for asynchronous handling of specified rout
-}
asyncRouter 
    : RequestChecker value 
    -> URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 value model 
    -> Router error1 value model
asyncRouter = 
    router stateLessAsync


{-| Router combinator for synchronous handling of specified rout 
which involves access or modification of the server state
-}
syncStateRouter 
    : RequestChecker value 
    -> URL 
    -> PathHandler value (IncompliteStateHandler value model error) 
    -> Router error value model 
    -> Router error value model
syncStateRouter = 
    router toStateFullSync


{-| Router combinator for synchronous handling of specified rout 
which involves access or modification of the server state
-}
asyncStateRouter 
    : RequestChecker value 
    -> URL 
    -> PathHandler value (Task error1 (IncompliteStateHandler value model error1)) 
    -> Router error1 value model 
    -> Router error1 value model
asyncStateRouter = 
    router toStateFullAsync