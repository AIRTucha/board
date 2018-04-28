module Board.Router exposing (..)

import Pathfinder exposing (..)
import Result
import Task
import Debug exposing (log)
import Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.Param exposing (..)

type alias RoutHandler a b c = 
    (Params, ReqValue a Object ) ->  Mode b (Answer c)


type alias Router a b =
    Request a -> Mode b (Answer a)


toStateLess handler model =
    let 
        (newModel, answer) = handler model   
    in
        (newModel, Sync << StateLess <| answer)


toStateFull =
    StateFull << toStateLess


nextStateLessSync =
    stateLessSync << Next


stateFullSync =
    Sync << toStateFull
    

stateFullAsync stateHandler =
    stateHandler
        |> Task.map toStateFull
        |> Async


stateLessSync = 
    Sync << StateLess


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


useSyncState = factory useHandler stateFullSync


getSyncState = factory getHandler stateFullSync


postSyncState = factory postHandler stateFullSync


putSyncState = factory putHandler stateFullSync


deleteSyncState = factory deleteHandler stateFullSync


useState = factory useHandler stateFullAsync


getState = factory getHandler stateFullAsync


postState = factory postHandler stateFullAsync


putState = factory putHandler stateFullAsync


deleteState = factory deleteHandler stateFullAsync


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
 

factory parsePath mode url cur next req =
    let 
        answer = next req
    in
        case answer of    
            Sync result ->
                case result of
                    StateLess value ->
                        case value of 
                            Next newReq ->
                                try2Dispache parsePath mode cur url newReq
                            
                            other ->
                                answer

                    StateFull toState ->
                        state parsePath mode cur url toState
                            |> StateFull
                            |> Sync

            Async result ->
                result 
                    |> Task.andThen (try2DispacheAsync parsePath mode cur url)
                    |> Async


state parsePath mode cur url toState model =
    let
        (newModel, answer) = toState model
    in
        ( newModel
        , case answer of
            Sync result ->
                case result of
                    StateLess value ->
                        case value of 
                            Next newReq ->
                                try2Dispache parsePath mode cur url newReq
                            
                            Reply _ ->
                                answer

                            Redirect _ ->
                                answer

                    StateFull toState ->
                        toState
                            |> state parsePath mode cur url 
                            |> StateFull
                            |> Sync

            Async result ->
                result 
                    |> Task.andThen (try2DispacheAsync parsePath mode cur url)
                    |> Async
        )


try2DispacheAsync parsePath mode cur url answer =
    case answer of
        StateLess value ->
            case value of 
                Next req ->
                    case try2Dispache parsePath mode cur url req of
                        Sync dispacheValue ->
                            Task.succeed dispacheValue
                        
                        Async dispacheValue ->
                            dispacheValue
                
                other ->
                    Task.succeed answer

        StateFull toState ->
            toState
                |> state parsePath mode cur url 
                |> StateFull
                |> Task.succeed


try2Dispache parsePath mode cur url req =
    case parsePath req of
        Just (resul, path) ->
            case parse url path of
                Failure _ ->
                    Next req
                        |> StateLess
                        |> Sync
                
                value ->
                    case parsingResult2params value of
                        Ok params ->
                            cur (params, resul)
                                |> mode
                        
                        Err _ -> 
                            Next req
                                |> StateLess
                                |> Sync

        Nothing ->
            Next req
                |> StateLess
                |> Sync