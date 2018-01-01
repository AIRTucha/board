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

        
parseValue parse (head, tail) =
    parse head
        |> Result.map ( \ value -> ( value, tail ))


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