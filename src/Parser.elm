module Parser exposing(..)


type Template 
    = P String
    | Integer
    | Collection (List Template)

type Tree
    = Leaf Maybe 
    | Tree

-- p : String -> Template
-- p string =
--     Param string

-- f : String -> Template
-- f string =
--     Flat string

-- int : String -> Template
-- int string =
--     Integer string

parser : Template -> String
parser value =
    case value of
        P string -> 
            string

        Integer -> 
            "int"
        
        _ -> "col"
        
        -- Collection c ->
        --      c
        --         |> List.map parser
        --         |> List.foldr (++) "" 


(</>) : Template -> Template -> Template
(</>) t1 t2 =
    case t1 of
        Collection l1 ->
            case t2 of 
                Collection l2 ->
                    Collection <| l1 ++ l2
            
                v ->
                    Collection <| v :: l1

        v1 ->
            case t2 of
                Collection l2 ->
                    Collection <| v1 :: l2
                
                v2 ->
                    Collection <| v1 :: v2 :: []


