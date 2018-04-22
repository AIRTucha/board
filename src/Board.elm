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
        , update = 
            router 
                |> server
                |> update
        , subscriptions = subscriptions
        }


-- subscriptions : Model -> Sub Msg
subscriptions model =
    Server.listen 8080 Input Error


-- init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


update server message model =
    case message of
        Input request ->
            server model (log "req" request)

        Output response ->
            Server.send response
                |> (\_ -> ( model, Cmd.none) )
        
        Model toState req ->
            (model, Cmd.none)
            -- let 
            --     (newModel, answer) = toState model
            -- in
            --     ( newModel
            --     , liftMode answer 
            --         |> Task.attempt (result2output newModel req)
            --     )

        Error msg ->
            log msg (model, Cmd.none)


server router model req = 
    case router req of 
        Async task ->
            ( model
            , task 
                |> Task.attempt (result2output model req)
            )

        Sync value ->
            -- TODO
            (model, Cmd.none)

        State _ ->
            (model, Cmd.none)

liftMode mode =
    case mode of 
        Sync answer ->
            Task.succeed answer 
        
        Async task ->
            task
        
        State _ ->
            Task.succeed <| Redirect "ok"

-- result2output : Request a -> Result x (Answer a1) -> Msg
result2output model req ans =
    case ans of
        Ok value ->
            toOutput req value

        _ ->
            Error <| url req

toOutput req value=
    case value of
        Next newReq ->
            Output response
            
        Reply res ->
            Output res 

        Redirect path ->
            req
                |> setURL path
                |> Input

        -- State toState ->
        --     Model toState req

stateResultHanler req result =
    case  result of
        Ok model2state ->
            Model model2state req 
        
        Err str ->
            Error str

replyWithState model2res req model =
    let 
        (newModel, res) = model2res model 
    in 
        ( newModel
        , res
            |> Task.succeed
            |> Task.perform Output
        )  

redirectWithState model2str req model =
    let 
        (newModel, path) = model2str model 
    in 
        ( newModel
        , setURL path req
            |> Task.succeed
            |> Task.perform Input
        )


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