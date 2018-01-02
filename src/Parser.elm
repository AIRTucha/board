module Parser exposing(..)

import List exposing (reverse)
import String exposing(toFloat, toInt, dropLeft, left, length, uncons, cons, fromChar, startsWith)
import Dict exposing(..)
import Maybe
import Result

type SubURL 
    = ParsePath String
    | ParseFloat
    | ParseInt
    | ParseStr
    | ParseAny
    | ParseQuery
-- Query Params

type URL
    = URLNode SubURL 
    | URLFork Char SubURL URL

type URLValue
    = Interger Int
    | Floating Float
    | Str String
    | MultyValue (List URLValue)
    | Failure String
    | Query (Dict String String)
    | Succes

p : String -> URL
p string =
    URLNode <| ParsePath string

float : URL
float =
    URLNode ParseFloat

int : URL
int =
    URLNode ParseInt

str: URL 
str = 
    URLNode ParseStr

any: URL
any = 
    URLNode ParseAny

query : URL
query =
    URLNode ParseQuery

parser : URL -> String -> URLValue
parser value string =
    parsingLoop value [] string

parsingLoop : URL -> (List URLValue) -> String -> URLValue
parsingLoop url result string =
    case url of
        URLFork char sub nextURL ->
            case sub of
                ParsePath path ->
                    let 
                        fullPath = path ++ fromChar char
                        lengthWithSplitter = (1 + length path)
                    in
                        if startsWith fullPath string then
                            string
                                |> dropLeft lengthWithSplitter
                                |> parsingLoop nextURL result
                        else 
                            path ++ ( fromChar char ) ++ " is not " ++ ( left lengthWithSplitter string ) 
                                |> reportError result


                ParseFloat ->
                    string
                        |> break char 
                        |> Result.andThen (parseValue String.toFloat)
                        |> packValue Floating result
                        |> parseNext nextURL
                           

                ParseInt ->
                    string  
                        |> break char
                        |> Result.andThen (parseValue String.toInt)
                        |> packValue Interger result
                        |> parseNext nextURL
                
                ParseStr ->
                    string
                        |> break char
                        |> packValue Str result
                        |> parseNext nextURL
                    
                ParseAny ->
                    string
                        |> break char
                        |> ignorValue result
                        |> parseNext nextURL

                ParseQuery ->
                    string
                        |> break char
                        |> Result.andThen (parseValue parseQuery)
                        |> packValue Query result
                        |> parseNext nextURL


        URLNode node ->
            case node of
                ParsePath path ->
                    if string == path then
                        makeValue result 
                    else 
                        path ++ " is not " ++ string
                            |> reportError result 
                

                ParseFloat ->
                    string
                        |> packResult result String.toFloat Floating
                

                ParseInt ->
                    string
                        |> packResult result String.toInt Interger

                                
                ParseStr ->
                    string
                        |> packResult result Ok Str


                ParseAny ->
                    makeValue result 


                ParseQuery ->
                    string
                        |> packResult result parseQuery Query
        
        
parseValue parse (head, tail) =
    parse head
        |> Result.map ( \ value -> ( value, tail ))


parseQuery : String -> Result String (Dict String String)
parseQuery string =
    let 
        eqTuples =
            string
                |> String.split "&"
                |> List.map (break '=')
        eqs = eqTuples
            |> List.filterMap isOk 
    in
        if (List.length eqs) == (List.length eqTuples) then
            eqs
                |> Dict.fromList
                |> Ok
        else 
            eqTuples
                |> List.filterMap isErr
                |> List.foldr (++) ""
                |> Err


isOk value =
    case value of 
        Ok v -> Just v 

        Err _ -> Nothing

isErr value = 
    case value of
        Ok _ -> Nothing 


        Err e -> Just e


packValue packer result input =
    case input of
        Ok ( value, tail ) ->
            ( packer value :: result, tail)
                |> Ok
        
        Err error ->
            result 
                |> (::) ( Failure error )
                |> Err


ignorValue result input =
    case input of
        Ok ( _, tail ) ->
            (result, tail)
                |> Ok
        
        Err error ->
            result 
                |> (::) ( Failure error )
                |> Err


parseNext url result =
    case result of
        Ok ( value, tail ) ->
            parsingLoop url value tail
        
        Err url ->
            makeValue url


packResult result parser packer input =
    case parser input of 
        Ok value ->
            packer value :: result
                |> makeValue 
            
        Err error ->
            reportError result error

makeValue: (List URLValue) -> URLValue
makeValue list =
    case list of
        head :: [] ->
            head 

        head :: tail ->
            MultyValue <| reverse list 
        
        [] ->
            Succes


reportError: (List URLValue) -> String -> URLValue
reportError result error =
    result 
        |> (::) ( Failure error )
        |> makeValue


(</>): URL -> URL -> URL
(</>) = fork '/'

(<?>): URL -> URL -> URL
(<?>) = fork '?'

(<&>): URL -> URL -> URL
(<&>) = fork '&'

fork : Char -> URL -> URL -> URL
fork char url1 url2 =
    case url1 of
        URLFork char1 sub1 nextURL1 ->
            URLFork char1 sub1 <| fork char nextURL1 url2
        
        URLNode sub1 ->
            URLFork char sub1 <| url2
        

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