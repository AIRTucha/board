module Board.Router.Param exposing (parsingResult2params)

{-| @docs parsingResult2params
-}
import Pathfinder exposing (..)
import List exposing (map, reverse)
import Board.Shared exposing (Params(..))

{-| Convert Pathfinder parsing values to Result Params which are used for routing.
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

       Success ->
         Ok EmptyParam


{-| Specially handler conversion for MultiValue case
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