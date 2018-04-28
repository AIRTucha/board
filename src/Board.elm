module Board exposing (..)

import Result
import Task
import Server exposing (url)
import Debug exposing (..)
import Shared exposing (..)


board router conf =
    Platform.program
        { init = ( conf.state, Cmd.none )
        , update = update conf router 
        , subscriptions = subscriptions conf
        }


subscriptions conf _ =
    Server.listen conf.portNumber Input Error    


update conf router message model =
    case message of
        Input request ->
            ( model
            , case router request of 
                Async task ->
                    task 
                        |> Task.attempt (resultToOutput request)
        
                Sync value ->
                    Task.succeed value 
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


resultToOutput req result =
    case result of
        Ok value ->
            toOutput req value

        Err msg ->
            Error msg


toOutput req state =
    case state of
        StateLess answer ->
            case answer of
                Next newReq ->
                    Output response
                    
                Reply res ->
                    Output res

                Redirect path ->
                    req
                        |> setURL path
                        |> Input
        
        StateFull stateHandler ->
             Model stateHandler req


setURL path req =
    case req of 
        Get value ->
            Get { value | url = path }

        Post value ->
            Post { value | url = path }

        Put value ->
            Put { value | url = path }

        Delete value ->
            Delete { value | url = path }