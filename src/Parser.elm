module Parser exposing(..)

import List exposing (reverse)
import String exposing(toFloat, toInt, dropLeft, left, length, uncons, cons, fromChar, startsWith)
import Maybe
import Result

type SubURL 
    = ParsePath String
    | ParseFloat
    | ParseInt
-- Any
-- Query
-- Query Params

type URL
    = URLNode SubURL 
    | URLFork Char SubURL URL

type URLValue
    = Interger Int
    | Floating Float
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
                    case (parseValue char String.toFloat string) of
                        Ok ( tail, float ) ->
                            parsingLoop nextURL ((Floating float) :: result) tail
                        
                        Err error ->
                            reportError result error
                           
                ParseInt ->
                    case parseValue char String.toInt string of
                        Ok (tail, int) ->
                           parsingLoop nextURL ((Interger int) :: result) tail
                        
                        Err error ->
                            reportError result error

        URLNode node ->
            case node of
                ParsePath path ->
                    if string == path then
                        makeValue result 
                    else 
                        path ++ " is not " ++ string
                            |> reportError result 
                
                ParseFloat ->
                    case String.toFloat string of
                        Ok float ->
                            (Floating float) :: result
                                |> makeValue 
                        
                        Err error ->
                            reportError result error
                
                ParseInt ->
                    case String.toInt string of
                        Ok int ->
                            (Interger int) :: result
                                |> makeValue 
                        
                        Err error ->
                            reportError result error

        
parseValue: Char -> (String -> Result String a) -> String -> Result String ( String, a )
parseValue char parse string =
    case (break char string) of
        Just (head, tail) ->
            parse head
                |> Result.map ( \ value -> ( tail, value ))
        
        Nothing ->
            Err <| string ++ " do not contain " ++ (fromChar char)


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