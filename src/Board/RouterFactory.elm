module Board.RouterFactory exposing (..)

import Pathfinder exposing (..)
import Result
import Task
import Board.Shared exposing (..)
import Basics exposing (..)
import Board.Param exposing (parsingResult2params)


toStateLess handler model =
    let 
        (newModel, answer) = handler model   
    in
        (newModel, Sync << StateLess <| answer)
    

toStateFullAsync stateHandler =
    stateHandler
        |> Task.map toStateFull
        |> Async


stateLessSync = 
    Sync << StateLess

nextStateLessSync =
    stateLessSync << Next


toStateFull =
    StateFull << toStateLess


stateFullSync =
    Sync << StateFull


toStateFullSync =
    Sync << toStateFull


stateLessAsync v = 
    v 
        |> Task.map StateLess
        |> Async

router parsePath mode url handler router request =
    request
        |> router 
        |> processModeAnswer parsePath mode url handler


processModeAnswer parsePath mode url handler modeAnswer =
    case modeAnswer of    
        Sync answer ->
            case answer of
                StateLess value ->
                    case value of 
                        Next newRequest ->
                            tryToProcessRequest parsePath mode handler url newRequest
                        
                        _ ->
                            modeAnswer

                StateFull stateHandler ->
                    stateHandler
                        |> toStateHandler parsePath mode handler url 
                        |> stateFullSync

        Async taskAnswer ->
            taskAnswer 
                |> Task.andThen (processAsyncAnswer parsePath mode handler url)
                |> Async


toStateHandler parsePath mode handler url stateHandler model =
    let
        (newModel, answer) = stateHandler model
    in
        ( newModel
        , processModeAnswer parsePath mode url handler answer
        )


processAsyncAnswer parsePath mode handler url answer =
    case answer of
        StateLess value ->
            case value of 
                Next request ->
                    request
                        |> tryToProcessRequest parsePath mode handler url
                        |> liftToAsync
                
                _ ->
                    Task.succeed answer

        StateFull stateHandler ->
            stateHandler
                |> toStateHandler parsePath mode handler url 
                |> StateFull
                |> Task.succeed


tryToProcessRequest parsePath mode handler url request =
    if parsePath request then
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