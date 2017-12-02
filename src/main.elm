module App exposing (log)

import Native.Console
import Native.Server
import Basics exposing (sqrt, pi)
import Platform exposing (program)

run _ = program
        { init = ( (), Cmd.none )
        , update = \() -> \() -> ( (), Cmd.none )
        , subscriptions = \() -> Sub.none
        }

-- main = np ( Native.Server.serve() )

{-
 Log data to console
-}
log value = Native.Console.println value 


-- type Shape
--     = Rect 
--     | Circle
--     | Trignle

-- check : Shape -> String
-- check shape = 
--     case shape of
--         Rect ->
--             "rect"

--         Circle ->
--             "circle"
        
--         Trignle ->
--             "triangle"

-- main = run ( log << check <| Rect )

type Shape
    = Rect Float Float
    | Circle Float
    | Triangle Float Float Float

space : Shape -> Float
space shape = 
    case shape of 
        Rect x y ->
            x * y
        
        Circle radius ->
            radius * pi * pi
        
        Triangle a b c ->
            let 
                p = (a + b + c) /2
            in 
                sqrt p * (p - a) * (p - b) * (p - c) 

main = run <| log << space <| Circle 2