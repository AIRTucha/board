module Parser exposing(..)

import List exposing (reverse)
import String exposing(toFloat, toInt, dropLeft, left, length, uncons, cons, fromChar, startsWith)
import Maybe
import Result

type SubURL 
    = ParsePath String
    | ParseFloat
    | ParseInt
    | ParseStr
    | ParseAny
-- Query
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
                    parseValue char String.toFloat string
                        |> packValue Floating result
                        |> parseNext nextURL
                           

                ParseInt ->
                    parseValue char String.toInt string
                        |> packValue Interger result
                        |> parseNext nextURL
                
                ParseStr ->
                    parseValue char Ok string
                        |> packValue Str result
                        |> parseNext nextURL
                    
                ParseAny ->
                    parseValue char Ok string
                        |> ignorValue result
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

        
parseValue: Char -> (String -> Result String a) -> String -> Result String ( String, a )
parseValue char parse string =
    case  break char string of
        Just (head, tail) ->
            parse head
                |> Result.map ( \ value -> ( tail, value ))
        
        Nothing ->
            Err <| string ++ " does not contain " ++ (fromChar char)


packValue packer result input =
    case input of
        Ok ( tail, value ) ->
            (tail, packer value :: result)
                |> Ok
        
        Err error ->
            result 
                |> (::) ( Failure error )
                |> Err


ignorValue result input =
    case input of
        Ok ( tail, _ ) ->
            (tail, result)
                |> Ok
        
        Err error ->
            result 
                |> (::) ( Failure error )
                |> Err


parseNext url result =
    case result of
        Ok ( tail, value ) ->
            parsingLoop url value tail
        
        Err url ->
            makeValue url

packResult
    : List URLValue
    -> (a -> Result String b)
    -> (b -> URLValue)
    -> a
    -> URLValue
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

break: Char -> String -> Maybe ( String, String )
break char string =
    splitOnce char "" string

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