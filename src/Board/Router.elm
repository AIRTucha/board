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


type alias Router a b =
    Request a -> Mode b (Answer a)


empty: Request a -> Mode b (Answer a model error)
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
 
factory parsePath mode url cur next req =
    case next req of    
        Sync result ->
            case result of
                Next newReq ->
                    try2Dispache parsePath mode cur url newReq
                
                Reply _ ->
                    Sync result 

                Redirect _ ->
                    Sync result

                State toState ->
                    Sync <| State <| state parsePath mode cur url req toState

        Async result ->
            result 
                |> Task.andThen (try2DispacheAsync parsePath mode cur url)
                |> Async


state parsePath mode cur url req toState model =
    let
        (newModel, answer) = toState model
    in
        case answer of
            Sync result ->
                case result of
                    Next newReq ->
                        (newModel, try2Dispache parsePath mode cur url newReq)
                    
                    Reply _ ->
                        (newModel, Sync result)

                    Redirect _ ->
                        (newModel, Sync result)

                    State toState ->
                        (newModel, Sync result)

            Async result ->
                result 
                    |> Task.andThen (try2DispacheAsync parsePath mode cur url)
                    |> Async
                    |> (\ t -> (newModel, t))


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