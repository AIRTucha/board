module Board exposing (..)

{-| 
@docs board
    , subscriptions
    , update
    , resultToOutput
    , toOutput
-}

import Result
import Task
import Server exposing (..)
import Debug exposing (..)
import Board.Shared exposing (..)
import Board.Internals exposing (..)
import Json.Decode exposing (..)


{-|
-}
board : (Request value -> Mode String (Answer value a String)) -> { b | errorPrefix : Maybe String , options : { https : HTTPSOptions, portNumber : Int, timeout : Int } , state : a } -> ((String -> Msg value1 model error) -> Sub (Msg value a String)) -> Program Never a (Msg value a String)
board router conf sub =
    Platform.program
        { init = ( conf.state, Cmd.none )
        , update = update conf router 
        , subscriptions = subscriptions conf sub
        }




{-|
-}
subscriptions : { a | options : { https : HTTPSOptions, portNumber : Int, timeout : Int } } -> ((String -> Msg value model error) -> Sub msg) -> b -> Sub msg
subscriptions conf sub _ =
    Sub.batch
        [ Server.listen conf.options
        , sub Test
        ]
    

{-|
-}
update : { a | errorPrefix : Maybe String } -> (Request value -> Mode String (Answer value b String)) -> Msg value b String -> b -> ( b, Cmd (Msg value b String) )
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

        Test str ->
            Debug.log "ok" str 
                |> \_ -> (model, Cmd.none)


{-|
-}
resultToOutput : { cargo : Object , content : Content value , cookies : Object , host : String , id : String , ip : String , method : Method , protocol : Protocol , time : Int , url : String } -> Result String (Answer value model error) -> Msg value model error
resultToOutput request result =
    case result of
        Ok answer ->
            toOutput request answer

        Err msg ->
            Error msg


{-|
-}
toOutput : { cargo : Object , content : Content value , cookies : Object , host : String , id : String , ip : String , method : Method , protocol : Protocol , time : Int , url : String } -> Answer value model error -> Msg value model error
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