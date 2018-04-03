module Board exposing (..)

import Pathfinder exposing (..)
import Dict exposing (..)
import Result
import List exposing (map, reverse)
import Task
import Server exposing (url)
import Debug exposing (log)
import Board.Router exposing(..)
import Shared exposing (..)
import Platform.Sub exposing (none)

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
    Server.listen 8080 (\v -> v)
    

type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )

update server message model =
    case message of
        Input request ->
            (model, server request)

        Output response ->
            Server.send response
                |> (\_ -> ( model, Cmd.none) )
            
        Error msg ->
            log msg (model, Cmd.none)
                    
server : (Request a -> Mode x (Answer a1)) -> Request a -> Cmd Msg
server router req = 
    case router req of 
        Async task ->
            task 
                |> Task.attempt (result2output req)

        Sync value ->
            Cmd.none

result2output : Request a -> Result x (Answer a1) -> Msg
result2output req res =
    case res of
        Ok (Reply value) ->
            Output value  
        
        _ ->
            Error <| url req

