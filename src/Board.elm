module Board exposing (..)

import Pathfinder exposing (..)
import Request exposing (..)
import Response exposing (..)
import Dict exposing (..)
import Result exposing (..)
import List exposing (map, reverse)

type HandlingResult
    = Contenue Request
    | Finish


type alias ReqHandler a = 
    (Params, a) -> Response


type alias Mid =
    Request -> Response


-- use: URL -> ReqHandler -> Mid -> Mid
use = factory useHandler


get = factory getHandler


post = factory postHandler


put = factory putHandler


delete = factory deleteHandler

 
factory: (Request -> Maybe (a, String)) -> URL -> ReqHandler a -> Mid -> Mid
factory parsePath url cur next req =
    case next req of
        Next newReq ->
            try2Dispache parsePath newReq cur url

        TaskNext reqTask ->
            reqTask
                |> andThen (try2Dispache parsePath cur url)
        next -> 
            next
        

try2Dispache parsePath req cur url =
    case parsePath req of
        Just (resul, path) ->
            case parse url path of
                Failure _ ->
                    Next req
                
                value ->
                    case parsingResult2params value of
                        Ok params ->
                            cur (params, resul)
                        
                        Err _ -> 
                            Next req

        Nothing ->
            Next req

useHandler: Request -> Maybe (Request, String)
useHandler req =
    case req of
        Get body ->
            Just (req, body.url)
        
        Post body ->
            Just (req, body.url)
        
        Put body ->
            Just (req, body.url)
        
        Delete body ->
            Just (req, body.url)

getHandler: Request -> Maybe (Body, String)
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

postHandler: Request -> Maybe (Body, String)
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

putHandler: Request -> Maybe (Body, String)
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

deleteHandler: Request -> Maybe (Body, String)
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