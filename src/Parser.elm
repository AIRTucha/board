module Parser exposing(..)



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
        
        -- Collection c ->
        --      c
        --         |> List.map parser
        --         |> List.foldr (++) "" 

(</>): URL -> URL -> URL
(</>) = fork '/'

fork : Char -> URL -> URL -> URL
fork char url1 url2 =
    case url1 of
        URLFork char1 sub1 nextURL1 ->
            URLFork char1 sub1 <| fork char nextURL1 url2
        
        URLNode sub1 ->
            URLFork char sub1 <| url2
                
                
    -- case t1 of
    --     URLFork f1 ->
    --         case t2 of 
    --             Collection l2 ->
    --                 Collection <| l1 ++ ParsePath "/" :: l2
            
    --             v ->
    --                 Collection <| v :: ParsePath "/" :: l1

    --     v1 ->
    --         case t2 of
    --             Collection l2 ->
    --                 Collection <| v1 :: ParsePath "/" :: l2
                
    --             v2 ->
    --                 Collection <| v1 :: ParsePath "/" :: v2 :: []


