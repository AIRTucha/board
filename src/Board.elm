module Board exposing (..)

import Pathfinder exposing (..)
import Dict exposing (..)
import Result
import List exposing (map, reverse)
import Task
import Server exposing (Request(..), Response, ReqValue, url)
import Debug exposing (log)
import Board.Router exposing(..)

board router =
    Platform.program
        { init = init
        , update = router 
            |> server
            |> update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Server.listen 8080 Input
    

type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


type Msg
    = Input Server.Message
    | Output (Result String Response)
 

update server message model =
    case message of
        Input request ->
            case request of 
                Ok req ->
                        (model, server req)

                Err msg ->
                    log msg ( model, Cmd.none)

        Output response ->
            case response of
                Ok res ->
                    Server.send res
                        |> (\_ -> ( model, Cmd.none) )
            
                Err msg ->
                    log msg (model, Cmd.none)

server router req = 
    case router req of 
        Async task ->
            task 
                |> Task.map (dispacheTask req)
                |> Task.perform Output

        Sync value ->
            Cmd.none


dispacheTask req ans =
    case ans of 
        Reply res ->
            Ok res
        
        _ ->
            Err <| url req

