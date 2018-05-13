module Board.Param exposing (parsingResult2params, Params(..))

 {- Convert Pathfinder parsing values to Result Params which are used for routing.
 -}
import Pathfinder exposing (..)
import Dict exposing (..)
import List exposing (map, reverse)


type Params
    = IntParam Int
    | FloatParam Float
    | StrParam String
    | MultiParam (List Params)
    | QueryParam (Dict String String)
    | EmptyParam


{-|
-}
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


{-|
-}
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