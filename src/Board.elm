module Board exposing (program)

{-| 
@docs program
-}

import Result
import Task exposing (attempt, perform, succeed)
import Server exposing (..)
import Debug exposing (..)
import Board.Shared exposing (..)
import Board.Internals exposing (..)


{-| Starting point of Board program
-}
program 
    : (Request value -> Mode String (Answer value a String)) 
    -> Configurations a 
    -> ((String -> Msg value1 model error) 
    -> Sub (Msg value a String)) 
    -> Program Never a (Msg value a String)
program router conf sub =
    Platform.program
        { init = ( conf.state, Cmd.none )
        , update = update conf router 
        , subscriptions = subscriptions conf sub
        }


{-| Subscription to server by configuration
-}
subscriptions 
    : Configurations a 
    -> ((String -> Msg value model error) -> Sub msg) 
    -> b -> Sub msg
subscriptions conf sub _ =
    Sub.batch
        [ Server.listen conf.options
        , sub Test
        ]
    

{-| Handles different phases of server lifecircle
-}
update 
    : Configurations a 
    -> (Request value -> Mode String (Answer value b String)) 
    -> Msg value b String 
    -> b 
    -> ( b, Cmd (Msg value b String) )
update conf router message model =
    let 
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
                            Output <| getResponse newReq 
                            
                        Reply res ->
                            Output res

                        Redirect path ->
                            { request | url = path }
                                |> Input
                
                StateFull stateHandler ->
                    Model stateHandler request
    in
        case message of
            Input request ->
                ( model
                , case router request of 
                    Async taskAnswer ->
                        taskAnswer 
                            |> attempt (resultToOutput request)
            
                    Sync answer ->
                        answer
                            |> succeed  
                            |> perform (toOutput request)
                )

            Output response ->
                send response
                    => ( model, Cmd.none)
            
            Model stateHandler request ->
                let 
                    (newModel, answer) = stateHandler model
                in
                    ( newModel
                    , answer 
                        |> liftToAsync
                        |> attempt (resultToOutput request)
                    )

            Error msg ->
                case conf.errorPrefix of
                    Just prefix ->
                        Debug.log prefix msg
                            => (model, Cmd.none)
                    
                    Nothing ->
                        (model, Cmd.none)

            Test str ->
                Debug.log "ok" str 
                    |> \_ -> (model, Cmd.none)