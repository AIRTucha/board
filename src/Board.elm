module Board exposing (..)

import Pathfinder exposing (..)
import Request exposing (..)
import Dict exposing (..)
import Result exposing (..)
import List exposing (map)

type HandlingResult
    = Contenue Request
    | Finish

type alias Handler a = 
    (a, Request) -> HandlingResult

type alias Mid =
    Request -> HandlingResult
-- type Board a
--     = Default (Handler a)
--     | Custom URL (Handler a)

-- mid: URL -> Handler a -> Request -> Maybe ()
-- mid url handler req =
--     let 
--         body = getBody url
--     in
--         case parse body.url of 
--             a -> 
--                 handler (body, req)
--                     |> Just 

--             _ -> Nothing

use: URL -> Handler ParsingResult -> Mid -> Request -> HandlingResult
use url cur next req =
    let 
        body = getBody req
    in
        case parse url body.url of

            Failure _ ->
                next req
            
            value ->
                cur (value, req)

a: URL -> String -> (ParsingResult -> HandlingResult) -> HandlingResult
a url value handler = 
    case parse url value of 

        Failure _ ->
            Finish 
        
        value ->
            handler value


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
            Ok <| MultiParam params
        
        head :: tail ->
            case parsingResult2params head of
                Ok value ->
                    value :: params
                        |> multiValue2Param tail
                
                Err err -> Err err