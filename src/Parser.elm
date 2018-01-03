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
                    string 
                        |> break char 
                        |> Result.andThen (checkEqual path)
                        |> ignorValue result
                        |> parseNext nextURL


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
                        |> Result.map ( \(value, tail) -> tail )
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
                    checkEqual path ( string, "" )
                        |> ignorValue result
                        |> packResult2
                

                ParseFloat ->
                    parseValue String.toFloat ( string, "" )
                        |> packValue Floating result
                        |> packResult2
                

                ParseInt ->
                    parseValue String.toInt ( string, "" )
                        |> packValue Interger result
                        |> packResult2

                                
                ParseStr ->
                    parseValue Ok ( string, "" )
                        |> packValue Str result
                        |> packResult2 


                ParseAny ->
                    makeValue result 


                ParseQuery ->
                    parseValue parseQuery ( string, "" )
                        |> packValue Query result
                        |> packResult2
        
        
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
            result 
                |> (::) ( Failure error )
                |> Err


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
            result 
                |> (::) ( Failure error )
                |> Err


parseNext url result =
    case result of
        Ok ( value, tail ) ->
            parsingLoop url value tail
        
        Err url ->
            makeValue url
            

packResult2 result =
    case result of 
        Ok (value, _) ->
            value
                |> makeValue 
            
        Err error ->
            error 
                |> makeValue 


makeValue: (List URLValue) -> URLValue
makeValue list =
    case list of
        head :: [] ->
            head 

        head :: tail ->
            MultyValue <| reverse list 
        
        [] ->
            Succes


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