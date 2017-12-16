module Parser exposing(..)


type Template 
    = Flat String
    | Param String
    | Collection (List Template)

type Tree
    = Leaf Maybe 
    | Tree

p : String -> Template
p string =
    Param string

f : String -> Template
f string =
    Flat string

parser : Template -> String
parser value =
    case value of
        Param string -> 
            "param" ++ string

        Flat value -> 
            value
        
        Collection c ->
             c
                |> List.map parser
                |> List.foldr (++) "" 


(</>) : Template -> Template -> Template
(</>) t1 t2 =
    case t1 of
        Collection l1 ->
            case t2 of 
                Collection l2 ->
                    Collection (l1 ++ l2)
            
                v ->
                    Collection (t1 :: t2 :: [])

        v ->
            t2 </> t1


