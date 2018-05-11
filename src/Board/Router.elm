module Board.Router exposing (..)

import Pathfinder exposing (..)
import Result
import Task
import Debug exposing (log)
import Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.Param exposing (..)
import File exposing(read, getContentType)
import Status exposing (..)
import Dict

type alias RoutHandler a b c = 
    (Params, Request a ) ->  Mode b (Answer c)


type alias Router a b =
    Request a -> Mode b (Answer a)


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


empty req =
    nextStateLessSync req


logger tag req =
    req
        |> logUrl tag
        |> nextStateLessSync


logUrl: String -> Request a -> Request a
logUrl tag req =
    Debug.log tag (reqToMsg req)
        => req


reqToMsg: Request a -> String
reqToMsg req =
    case req.method of 
        Get ->
            "GET " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Post ->
            "POST " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Put ->
            "PUT " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Delete ->
            "DELETE " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)


fromatDate req =    
    req.time
        |> toFloat
        |> fromTime
        |> Basics.toString
        

useHandler: Request a -> Bool
useHandler req =
    True


getHandler: Request a -> Bool
getHandler req =
    req.method == Get


postHandler: Request a -> Bool
postHandler req =
    req.method == Post


putHandler: Request a -> Bool
putHandler req =
    req.method == Put


deleteHandler: Request a -> Bool
deleteHandler req =
    req.method == Delete


useSyncState = factory useHandler toStateFullSync


getSyncState = factory getHandler toStateFullSync


postSyncState = factory postHandler toStateFullSync


putSyncState = factory putHandler toStateFullSync


deleteSyncState = factory deleteHandler toStateFullSync


useState = factory useHandler toStateFullAsync


getState = factory getHandler toStateFullAsync


postState = factory postHandler toStateFullAsync


putState = factory putHandler toStateFullAsync


deleteState = factory deleteHandler toStateFullAsync


useSync = factory useHandler stateLessSync


getSync = factory getHandler stateLessSync


postSync = factory postHandler stateLessSync


putSync = factory putHandler stateLessSync


deleteSync = factory deleteHandler stateLessSync


use = factory useHandler stateLessAsync


get = factory getHandler stateLessAsync


post = factory postHandler stateLessAsync


put = factory putHandler stateLessAsync


delete = factory deleteHandler stateLessAsync
 
static basePath prefix router =
    router
        |> get (basePath </> str) (getFile prefix)


getFile prefix (param, req) =
    let 
        next = req
            |> Next
            |> Task.succeed
    in
        case param of 
            StrParam path ->
                prefix ++ path
                    |> read
                    |> Task.map (makeResponse path req)
                    |> Task.map Reply
                    |> Task.onError (onGetFileError next)
            
            _ ->
                next


onGetFileError value _ =
    value


makeResponse path req file = 
    { response
    | content = Data (getContentType path) file
    , status = custom 200
    , header =  Dict.insert "Server" "test" <| Dict.insert "Cache-Control" "public" response.header
    , id = req.id
    }


factory parsePath mode url handler router request =
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