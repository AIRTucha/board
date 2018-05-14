module Board.Router.Factory exposing (..)

import Pathfinder exposing (..)
import Result
import Task
import Basics exposing (..)
import Board.Router.Param exposing (parsingResult2params)
import Board.Internals exposing (..)


{-|
-}
toStateLess handler model =
    let 
        (newModel, answer) = handler model   
    in
        (newModel, Sync << StateLess <| answer)
    

{-|
-}
toStateFullAsync stateHandler =
    stateHandler
        |> Task.map toStateFull
        |> Async

{-|
-}
stateLessSync = 
    Sync << StateLess


{-|
-}
nextStateLessSync =
    stateLessSync << Next


{-|
-}
toStateFull =
    StateFull << toStateLess


{-|
-}
stateFullSync =
    Sync << StateFull


{-|
-}
toStateFullSync =
    Sync << toStateFull


{-|
-}
stateLessAsync v = 
    v 
        |> Task.map StateLess
        |> Async



router checkMethod mode url handler router request =
    request
        |> router 
        |> processModeAnswer checkMethod mode url handler


processModeAnswer checkMethod mode url handler modeAnswer =
    case modeAnswer of    
        Sync answer ->
            case answer of
                StateLess value ->
                    case value of 
                        Next newRequest ->
                            tryToProcessRequest checkMethod mode handler url newRequest
                        
                        _ ->
                            modeAnswer

                StateFull stateHandler ->
                    stateHandler
                        |> toStateHandler checkMethod mode handler url 
                        |> stateFullSync

        Async taskAnswer ->
            taskAnswer 
                |> Task.andThen (processAsyncAnswer checkMethod mode handler url)
                |> Async


toStateHandler checkMethod mode handler url stateHandler model =
    let
        (newModel, answer) = stateHandler model
    in
        ( newModel
        , processModeAnswer checkMethod mode url handler answer
        )


processAsyncAnswer checkMethod mode handler url answer =
    case answer of
        StateLess value ->
            case value of 
                Next request ->
                    request
                        |> tryToProcessRequest checkMethod mode handler url
                        |> liftToAsync
                
                _ ->
                    Task.succeed answer

        StateFull stateHandler ->
            stateHandler
                |> toStateHandler checkMethod mode handler url 
                |> StateFull
                |> Task.succeed


tryToProcessRequest checkMethod mode handler url request =
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