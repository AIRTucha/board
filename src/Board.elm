module Board exposing (..)

import Result
import Task
import Server exposing (..)
import Debug exposing (..)
import Shared exposing (..)


board router conf =
    Platform.program
        { init = ( conf.state, Cmd.none )
        , update = update conf router 
        , subscriptions = subscriptions conf
        }


subscriptions conf _ =
    Server.listen conf.portNumber conf.httpsOptions


update conf router message model =
    case message of
        Input request ->
            ( model
            , case router request of 
                Async taskAnswer ->
                    taskAnswer 
                        |> Task.attempt (resultToOutput request)
        
                Sync answer ->
                    answer
                        |> Task.succeed  
                        |> Task.perform (toOutput request)
            )

        Output response ->
            Server.send response
                => ( model, Cmd.none)
        
        Model stateHandler request ->
            let 
                (newModel, answer) = stateHandler model
            in
                ( newModel
                , answer 
                    |> liftToAsync
                    |> Task.attempt (resultToOutput request)
                )

        Error msg ->
            case conf.errorPrefix of
                Just prefix ->
                    Debug.log prefix msg
                        => (model, Cmd.none)
                
                Nothing ->
                    (model, Cmd.none)


resultToOutput request result =
    case result of
        Ok answer ->
            toOutput request answer

        Err msg ->
            Error msg


toOutput request state =
    case state of
        StateLess answer ->
            case answer of
                Next newReq ->
                    Output response
                    
                Reply res ->
                    Output res

                Redirect path ->
                    { request | url = path }
                        |> Input
        
        StateFull stateHandler ->
             Model stateHandler request