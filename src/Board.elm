module Board exposing (..)

import Pathfinder exposing (..)
import Request exposing (..)
import Dict exposing (..)
import Result exposing (..)
import List exposing (map, reverse)

type HandlingResult
    = Contenue Request
    | Finish


type alias Handler = 
    (Params, Request) -> HandlingResult


type alias Mid =
    Request -> HandlingResult


use: URL -> Handler -> Mid -> Request -> HandlingResult
use url cur next req =
    let 
        body = getBody req
    in
        case parse url body.url of
            Failure _ ->
                next req
            
            value ->
                case parsingResult2params value of
                    Ok params ->
                        cur (params, req)
                    
                    Err _ -> next req


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