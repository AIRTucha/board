module Board.Router exposing (..)

import Pathfinder exposing (..)
import Dict exposing (..)
import Result
import List exposing (map, reverse)
import Task
import Server exposing (url)
import Debug exposing (log)
import Shared exposing (..)

-- Model
-- All paths

type alias RoutHandler a b c = 
    (Params, ReqValue a Object ) ->  Mode b (Answer c)

stateSync value =
    AsyncState <| stateHelper value

stateHelper value model =
    let 
        (newModel, answer) = value model 
    in
        (newModel, Sync <| StateLess answer)

type alias Router a b =
    Request a -> Mode b (Answer a)


-- empty: Request a -> Mode b (Answer a model error)/
empty req =
    Next req
        |> StateLess
        |> Sync

stateFullSync =
    Sync << StateFull

stateFullAsync v =
    v 
        |> Task.map StateFull
        |> Async

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

stateLessSync = 
    Sync << StateLess

stateLessAsync v = 
    v 
        |> Task.map StateLess
        |> Async

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
                        state parsePath mode cur url (stateHelper toState)
                            |> AsyncState
                            |> Sync

                    AsyncState toState ->
                        state parsePath mode cur url toState
                            |> AsyncState
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
                        state parsePath mode cur url (stateHelper toState)
                            |> AsyncState
                            |> Sync

                    AsyncState toState ->
                        state parsePath mode cur url toState
                            |> AsyncState
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
                    answer
                        |> Task.succeed

              
        StateFull toState ->
            state parsePath mode cur url (stateHelper toState)
                |> AsyncState
                |> Task.succeed

        AsyncState toState ->
            state parsePath mode cur url toState
                |> AsyncState
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


type Params
    = IntParam Int
    | FloatParam Float
    | StrParam String
    | MultiParam (List Params)
    | QueryParam (Dict String String)
    | EmptyParam


parsingResult2params: ParsingResult -> Result String Params
parsingResult2params result =
    case result of 
       Integer int ->
         Ok <| IntParam int

       Floating float ->
         Ok <| FloatParam float 

       Str str ->
         Ok <| StrParam str 
    
       Failure str ->
         Err str

       MultiValue list ->
         multiValue2Param list []

       Query dict ->
         Ok <| QueryParam dict

       Succes ->
         Ok EmptyParam


multiValue2Param: List ParsingResult -> List Params -> Result String Params
multiValue2Param list params =
    case list of
        [] -> 
            Ok <| MultiParam (reverse params)
        
        head :: tail ->
            case parsingResult2params head of
                Ok value ->
                    value :: params
                        |> multiValue2Param tail
                
                Err err -> Err err