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
type Answer a 
    = Redirect String
    | Reply (Response a)
    | Next (Request a)
    -- | StateRedirect ()


type Mode a b
    = Async (Task.Task a b)
    | Sync b


type alias RoutHandler a b c = 
    (Params, ReqValue a Object ) ->  Mode b (Answer c)


type alias Router a b =
    Request a -> Mode b (Answer a)


empty: Request a -> Mode b (Answer a)
empty req =
    Sync <| Next req


useSync = factory useHandler Sync


getSync = factory getHandler Sync


postSync = factory postHandler Sync


putSync = factory putHandler Sync


deleteSync = factory deleteHandler Sync


use = factory useHandler Async


get = factory getHandler Async


post = factory postHandler Async


put = factory putHandler Async


delete = factory deleteHandler Async
 
 
factory 
    : (Request a -> Maybe ( b, String )) 
    -> (c -> Mode a1 (Answer a)) 
    -> URL 
    -> (( Params, b ) -> c) 
    -> (d -> Mode a1 (Answer a)) 
    -> d 
    -> Mode a1 (Answer a)
factory parsePath mode url cur next req =
    case next req of    
        Sync result ->
            case result of
                Next newReq ->
                    try2Dispache parsePath mode cur url newReq
                
                Reply res ->
                    Sync result 

                Redirect string ->
                    Sync result 

        Async result ->
            result 
                |> Task.andThen (try2DispacheAsync parsePath mode cur url)
                |> Async
 

try2DispacheAsync parsePath mode cur url response =
    case response of
        Next req ->
            case try2Dispache parsePath mode cur url req of
                Sync value ->
                    Task.succeed value
                
                Async value ->
                    value
              
        other ->
            Task.succeed (response) 


try2Dispache parsePath mode cur url req =
    case parsePath req of
        Just (resul, path) ->
            case parse url path of
                Failure _ ->
                    Sync <| Next req
                
                value ->
                    case parsingResult2params value of
                        Ok params ->
                            mode <| cur (params, resul)
                        
                        Err _ -> 
                            Sync <| Next req

        Nothing ->
            Sync <| Next req


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