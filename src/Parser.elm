module Parser exposing(..)

import String exposing(uncons, cons, fromChar)
import Maybe

type SubURL 
    = ParsePath String
    | ParseFloat
    | ParseInt

type URL
    = URLNode SubURL 
    | URLFork Char SubURL URL

p : String -> URL
p string =
    URLNode <| ParsePath string

float : URL
float =
    URLNode ParseFloat

int : URL
int =
    URLNode ParseInt

-- parser : URL -> String
-- parser value =
--     case value of
--         P string -> 
--             string

--         Integer -> 
--             "int"
        
--         _ -> "col"
        
--         Collection c ->
--              c
--                 |> List.map parser
--                 |> List.foldr (++) "" 

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