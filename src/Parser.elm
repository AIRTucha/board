module Parser exposing(..)


type URL 
    = ParsePath String
    | ParseFloat
    | ParseInt
    | Collection (List URL)

type Tree
    = Leaf Maybe 
    | Tree

p : String -> URL
p string =
    ParsePath string

float : URL
float =
    ParseFloat

int : URL
int =
    ParseInt

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


(</>) : URL -> URL -> URL
(</>) t1 t2 =
    case t1 of
        Collection l1 ->
            case t2 of 
                Collection l2 ->
                    Collection <| l1 ++ ParsePath "/" :: l2
            
                v ->
                    Collection <| v :: ParsePath "/" :: l1

        v1 ->
            case t2 of
                Collection l2 ->
                    Collection <| v1 :: ParsePath "/" :: l2
                
                v2 ->
                    Collection <| v1 :: ParsePath "/" :: v2 :: []


