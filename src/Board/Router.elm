module Board.Router exposing (..)

import Pathfinder exposing (..)
import Result
import Task
import Debug exposing (log)
import Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.Param exposing (..)
import File exposing(read)

type alias RoutHandler a b c = 
    (Params, ReqValue a Object ) ->  Mode b (Answer c)


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
    case req of 
        Get body ->
            "GET " ++ body.url ++ " ip" ++ body.ip ++ " " ++ (fromatDate body)
        
        Post body ->
            "POST " ++ body.url ++ " ip" ++ body.ip ++ " " ++ (fromatDate body)
        
        Put body ->
            "PUT " ++ body.url ++ " ip" ++ body.ip ++ " " ++ (fromatDate body)
        
        Delete body ->
            "DELETE " ++ body.url ++ " ip" ++ body.ip ++ " " ++ (fromatDate body)


fromatDate req =    
    req.time
        |> toFloat
        |> fromTime
        |> Basics.toString
        

useHandler: Request a -> Maybe (ReqValue a Object, String)
useHandler req =
    case req of
        Get body ->
            Just (body, body.url)
        
        Post body ->
            Just (body, body.url)
        
        Put body ->
            Just (body, body.url)
        
        Delete body ->
            Just (body, body.url)


getHandler: Request a -> Maybe (ReqValue a Object, String)
getHandler req =
    case req of
        Get body ->
            Just (body, body.url)
        
        Post body ->
            Nothing
        
        Put body ->
            Nothing
        
        Delete body ->
            Nothing


postHandler: Request a -> Maybe (ReqValue a Object, String)
postHandler req =
    case req of
        Get body ->
            Nothing
        
        Post body ->
            Just (body, body.url)
        
        Put body ->
            Nothing
        
        Delete body ->
            Nothing


putHandler: Request a -> Maybe (ReqValue a Object, String)
putHandler req =
    case req of
        Get body ->
            Nothing
        
        Post body ->
            Nothing
        
        Put body ->
            Just (body, body.url)
        
        Delete body ->
            Nothing


deleteHandler: Request a -> Maybe (ReqValue a Object, String)
deleteHandler req =
    case req of
        Get body ->
            Nothing
        
        Post body ->
            Nothing
        
        Put body ->
            Nothing
        
        Delete body ->
            Just (body, body.url)


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
        next = Task.succeed <| Next <| Get req
    in
        case param of 
            StrParam path ->
                prefix ++ path
                    |> read
                    |> Task.map (makeResponse req)
                    |> Task.map Reply
                    |> Task.onError (\ _ -> next)
            
            _ ->
                next


makeResponse req file = 
    let 
        res = response
    in
        { res
        | content = Data "test" file
        , id = req.id
        } 


makeTextResponse req text = 
    let 
        res = response
    in
        { res
        | content = Text "text/plain" text
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
    case parsePath request of
        Just (newReq, path) ->
            case parse url path of
                Failure _ ->
                    nextStateLessSync request
                
                value ->
                    case parsingResult2params value of
                        Ok params ->
                            (params, newReq)
                                |> handler
                                |> mode
                        
                        Err _ -> 
                            nextStateLessSync request

        Nothing ->
            nextStateLessSync request