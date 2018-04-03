module Board exposing (..)

import Pathfinder exposing (..)
import Dict exposing (..)
import Result
import List exposing (map, reverse)
import Task
import Server exposing (Request(..), Response, ReqValue, url)
import Debug exposing (log)

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

type HandlingResult
    = Contenue Request
    | Finish

type Answer a
    = Redirect String
    | Reply Response
    | Next (Request a)


type Mode a b
    = Async (Task.Task a b)
    | Sync b

type alias RoutHandler a b c = 
    (Params, ReqValue a) ->  Mode b (Answer c)


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

-- use: URL -> RoutHandler -> Router-> Mid
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
                
                _ ->
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


useHandler: Request a -> Maybe (ReqValue a, String)
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


getHandler: Request a -> Maybe (ReqValue a, String)
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


postHandler: Request a -> Maybe (ReqValue a, String)
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


putHandler: Request a -> Maybe (ReqValue a, String)
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


deleteHandler: Request a -> Maybe (ReqValue a, String)
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