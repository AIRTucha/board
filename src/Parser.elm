module Parser exposing(..)

import List exposing (reverse)
import String exposing(toFloat, toInt, dropLeft, left, length, uncons, cons, fromChar, startsWith)
import Dict exposing(..)
import Maybe
import Result
import Tuple 

type SubURL 
    = ParsePath String
    | ParseFloat
    | ParseInt
    | ParseStr
    | ParseAny
    | ParseQuery


type URL
    = OrderedURL Char URL URL
    | UnorderedURL Char (List URL)
    | NodeURL SubURL


type URLValue
    = Interger Int
    | Floating Float
    | Str String
    | MultyValue (List URLValue)
    | Failure String
    | Query (Dict String String)
    | Succes

toString url =
    case url of 
        OrderedURL char url1 url2 ->
            "OrderedURL(" ++ (fromChar char) ++ " " ++ toString url1 ++ " " ++ toString url2 ++ ")"

        UnorderedURL char subUrls ->
            "UnorderedURL(" ++ (fromChar char) ++ " " ++ String.join " " (List.map toString subUrls) 

        NodeURL node ->
            case node of
                ParsePath path ->
                    "Path " ++ path   
                
                
                ParseFloat ->
                    "Float"


                ParseInt ->
                    "Int"
                
                
                ParseStr ->
                    "String"


                ParseAny ->
                    "Any"
    
    
                ParseQuery ->
                    "Query"


p : String -> URL
p string =
    NodeURL <| ParsePath string


float : URL
float =
    NodeURL ParseFloat


int : URL
int =
    NodeURL ParseInt


str: URL 
str = 
    NodeURL ParseStr


any: URL
any = 
    NodeURL ParseAny


query : URL
query =
    NodeURL ParseQuery


parser : URL -> String -> URLValue
parser value string =
    case parsingLoop value [] string Nothing of
        Ok ( result, "" ) ->
            makeValue result

        Ok (result, stringEnding ) ->
            Failure <| stringEnding ++ " is not specified"

        Err err ->
            Failure err

parsingLoop : URL -> (List URLValue) -> String -> Maybe Char -> Result (String) ( List URLValue, String )
parsingLoop url result string tailChar =
    case url of
        OrderedURL char suburl nextURL ->
            case parsingLoop suburl result string (Just char) of
                Ok (newResult, newString) ->
                    parsingLoop nextURL newResult newString tailChar

                a -> a
                

        NodeURL node ->
            case tailChar of
                Just char ->
                    string
                        |> break char
                        |> Result.andThen (parseNode result node)
                
                Nothing ->
                    parseNode result node (string, "")


        UnorderedURL char urls ->
            urls
                |> zipIndex
                |> parseUnordered char tailChar string [] [] []


parseUnordered: Char -> Maybe Char -> String -> List (Int, URL) -> List (Int, List URLValue) -> List (Int, List URLValue)  -> List (Int, URL) -> Result String (List URLValue, String)
parseUnordered char tailChar string prevUrls result curResult urls =
    case urls of 
        [] ->
            let
                newResult = (List.append curResult result)
            in
                case prevUrls of
                    [] ->
                        newResult
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
                            |> flattenUtilFailure []
                            |> Result.map (\ v -> ( v, string) )


                    head :: tail ->
                        if 0 < List.length curResult then 
                            parseUnordered char tailChar string [] newResult [] prevUrls
                        else
                            let
                                template = prevUrls
                                    |> List.sortBy Tuple.first
                                    |> List.map (Tuple.second >> toString)
                                    |> String.join " or "
                            in
                                
                            Err <| "Start of " ++ string ++ " does not have any value which can be corectly parsed by: " ++ template ++ ", separated by " ++ fromChar char ++ "."
                    

        (i, url) :: restOfUrls ->
            let 
                newTailChar = 
                    if restOfUrls == [] && prevUrls == [] then 
                        tailChar 
                    else 
                        Just char
            in
                case parsingLoop url [] string newTailChar of
                    Ok (newResult, restOfString) ->
                        parseUnordered char tailChar restOfString prevUrls result ((i, newResult) :: curResult) restOfUrls
                    
                    Err _ ->
                        parseUnordered char tailChar string ((i, url) :: prevUrls) result curResult restOfUrls


flattenUtilFailure: List URLValue -> List( List URLValue ) -> Result String (List URLValue)
flattenUtilFailure accum result =
    case result of 
        [] ->
            Ok accum
        
        head :: tail ->
            case findFailure head of
                Just err ->
                    Err err


                Nothing ->
                    flattenUtilFailure (List.append head accum) tail


findFailure result =
    case result of 
        [] ->
            Nothing
        
        (Failure err) :: tail ->
            Just err

        _ :: tail ->
            findFailure tail 


indices list =
    List.range 1 (List.length list)


zipIndex: List a -> List ( Int, a )
zipIndex list =
    list |>
      List.map2 (,) (indices list)


parseNode result node strings =
    case node of
        ParsePath path ->
            checkEqual path strings
                |> ignorValue result
        

        ParseFloat ->
            parseValue String.toFloat strings
                |> packValue Floating result
        

        ParseInt ->
            parseValue String.toInt strings
                |> packValue Interger result

                        
        ParseStr ->
            parseValue Ok strings
                |> packValue Str result


        ParseAny ->
            Ok (result, Tuple.second strings)


        ParseQuery ->
            parseValue parseQuery strings
                |> packValue Query result

parseValue parse (head, tail) =
    parse head
        |> Result.map ( \ value -> ( value, tail ))


parseQuery : String -> Result String (Dict String String)
parseQuery string =
    let 
        (oks, errs) =
            string
                |> String.split "&"
                |> List.map (break '=')
                |> partitionLift ( [], [] )
    in
        if (List.length errs) > 0 then
            errs
                |> String.concat
                |> String.append "Query is not correct: "
                |> Err
        else
            oks
                |> Dict.fromList
                |> Ok


partitionLift (succes, failure) list =
    case list of
        [] ->
            ( succes, failure )


        (Ok head) :: tail ->
            partitionLift ( head :: succes, failure ) tail
        

        (Err head) :: tail ->
            partitionLift ( succes, head :: failure ) tail


packValue packer result input =
    case input of
        Ok ( value, tail ) ->
            ( packer value :: result, tail)
                |> Ok
        
        Err error ->
            Err error


checkEqual path (string, tail) =
    if path == string then
        Ok tail
    else 
        Err ( path ++ " is not " ++ string)


ignorValue result input =
    case input of
        Ok tail ->
            (result, tail)
                |> Ok
        
        Err error ->
            Err error
            

makeValue: (List URLValue) -> URLValue
makeValue list =
    case list of
        head :: [] ->
            head 

        head :: tail ->
            MultyValue <| reverse list 
        
        [] ->
            Succes

-- (</>): URL -> URL -> URL
(</>) = orderedDevider '/'

(<=>) = orderedDevider '='

-- (<?>): URL -> URL -> URL
(<?>) = orderedDevider '?'


-- (<&>): URL -> URL -> URL
(<&>) = unorderedDevider '&'

(<*>) = unorderedDevider '*'


orderedDevider char url1 url2 =
    OrderedURL char url1 url2


unorderedDevider char url1 url2 =
    case url1 of
        OrderedURL _ a _ ->
            merge ( (::) url1 ) char url1 url2


        UnorderedURL char1 urls1 ->
            if char1 == char then 
                merge ( List.append urls1 ) char url1 url2
            else 
                merge ( (::) url1 ) char url1 url2
        

        NodeURL a ->
            merge ( (::) url1 ) char url1 url2


merge append char url1 url2 =
    UnorderedURL char <| 
        append <| 
            case url2 of
                UnorderedURL char2 urls2 ->
                    if char == char2 then
                        urls2 
                    else
                        [url2]
                
                _ ->
                    [url2] 
    

-- devider : ( Char -> SubURL -> URL -> URL ) -> Char -> SubURL -> URL -> URL
-- devider packer char url1 url2 =
--     case url2 of
--         OrderedURL char1 sub1 nextURL1 ->
--             OrderedURL char1 sub1 <| devider packer char nextURL1 url2

--         UnorderedURL char1 currURL nextURL ->
--             UnorderedURL char1 currURL <| devider packer char nextURL url2
        
--         URLNode sub1 ->
--             packer char sub1 url2

--         URLEnd ->
--             URLEnd


orderedPacker =
    OrderedURL 


-- unorderedPacker char sub1 url2 =
--     UnorderedURL char [sub1] url2


break: Char -> String -> Result String ( String, String )
break char string =
    case splitOnce char "" string of
        Just ( head, tail ) ->
            Ok ( head, tail )

        Nothing ->
            Err <| string ++ " does not contain " ++ (fromChar char)


splitOnce: Char -> String -> String -> Maybe ( String, String )
splitOnce char head tail =
    case uncons tail of 
        Just (first, rest) ->
            if first == char then 
                Just ( head, rest ) 
            else 
                splitOnce char (head ++ fromChar first) rest
        
        Nothing ->
            Nothing