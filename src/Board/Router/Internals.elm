module Board.Router.Internals exposing (..)

import Pathfinder exposing (..)
import Result
import Task
import Basics exposing (..)
import Board.Router.Param exposing (parsingResult2params)
import Board.Internals exposing (..)
import Board.Shared exposing (..)

{-|
-}
type alias PathHandler value answer = 
    ( Params , Request value ) -> answer 

{-|
-}
type alias Router error request value model = 
    request -> Mode error (Answer value model error)


{-|
-}
toStateLess : (a -> ( b, AnswerValue value model error )) -> a -> ( b, Mode error1 (Answer value model error) )
toStateLess handler model =
    let 
        (newModel, answer) = handler model   
    in
        (newModel, Sync << StateLess <| answer)
    

{-|
-}
toStateFullAsync : Task.Task error (model -> ( model, AnswerValue value model error1 )) -> Mode error (Answer value model error1)
toStateFullAsync stateHandler =
    stateHandler
        |> Task.map toStateFull
        |> Async

{-|
-}
stateLessSync : AnswerValue value model error -> Mode error1 (Answer value model error)
stateLessSync = 
    Sync << StateLess


{-|
-}
nextStateLessSync : Request value -> Mode error1 (Answer value model error)
nextStateLessSync =
    stateLessSync << Next


{-|
-}
toStateFull : (model -> ( model, AnswerValue value model error )) -> Answer value model error
toStateFull =
    StateFull << toStateLess


{-|
-}
stateFullSync : StateHandler value model error -> Mode error1 (Answer value model error)
stateFullSync =
    Sync << StateFull


{-|
-}
toStateFullSync : (model -> ( model, AnswerValue value model error )) -> Mode error1 (Answer value model error)
toStateFullSync =
    Sync << toStateFull


{-|
-}
stateLessAsync : Task.Task error (AnswerValue value model error1) -> Mode error (Answer value model error1)
stateLessAsync v = 
    v 
        |> Task.map StateLess
        |> Async


{-|
-}
router 
    : ModePacker answer value model error
    -> MethodChecker value
    -> URL 
    -> PathHandler value answer 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
router mode checkMethod url handler router request =
    request
        |> router 
        |> processModeAnswer mode checkMethod url handler


{-|
-}
processModeAnswer 
    : ModePacker answer value model error 
    -> MethodChecker value
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
                        |> toStateHandler mode checkMethod handler url 
                        |> stateFullSync

        Async taskAnswer ->
            taskAnswer 
                |> Task.andThen (processAsyncAnswer mode checkMethod handler url)
                |> Async


{-|
-}
toStateHandler 
    : ModePacker answer value model error
    -> MethodChecker value 
    -> PathHandler value answer
    -> URL 
    -> (model -> ( model, Mode error (Answer value model error) )) 
    -> model 
    -> ( model, Mode error (Answer value model error) )
toStateHandler mode checkMethod handler url stateHandler model =
    let
        (newModel, answer) = stateHandler model
    in
        ( newModel
        , processModeAnswer mode checkMethod url handler answer
        )


{-|
-}
processAsyncAnswer 
    :  ModePacker answer value model error
    -> MethodChecker value 
    -> PathHandler value answer
    -> URL 
    -> Answer value model error
    -> Task.Task error (Answer value model error)
processAsyncAnswer mode checkMethod handler url answer =
    case answer of
        StateLess value ->
            case value of 
                Next request ->
                    request
                        |> tryToProcessRequest mode checkMethod handler url
                        |> liftToAsync
                
                _ ->
                    Task.succeed answer

        StateFull stateHandler ->
            stateHandler
                |> toStateHandler mode checkMethod handler url 
                |> StateFull
                |> Task.succeed


{-|
-}
tryToProcessRequest 
    : ModePacker answer value model error 
    -> MethodChecker value
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


syncStateRouter = router toStateFullSync


asyncStateRouter = router toStateFullAsync


syncRouter = router stateLessSync


asyncRouter = router stateLessAsync