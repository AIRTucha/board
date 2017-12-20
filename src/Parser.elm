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


-- (</>) : URL -> URL -> URL
-- (</>) t1 t2 =
--     case t1 of
--         Collection l1 ->
--             case t2 of 
--                 Collection l2 ->
--                     Collection <| l1 ++ ParsePath "/" :: l2
            
--                 v ->
--                     Collection <| v :: ParsePath "/" :: l1

--         v1 ->
--             case t2 of
--                 Collection l2 ->
--                     Collection <| v1 :: ParsePath "/" :: l2
                
--                 v2 ->
--                     Collection <| v1 :: ParsePath "/" :: v2 :: []


