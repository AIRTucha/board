module App exposing (..)

import Native.Console
import Native.Server
import Basics exposing (sqrt, pi)
import Platform exposing (program)
import Path.Generic exposing (takeExtension)
import String exposing (toLower)
import Debug exposing(log)
import Task exposing(..)
import Native.File

import File exposing(..)

run _ = program
        { init = ( (), Cmd.none )
        , update = \() -> \() -> ( (), Cmd.none )
        , subscriptions = \() -> Sub.none
        }

urlParser url =
    if url == "/" then 
        "./public/index.html" 
    else 
        "./public" ++ url


typeParser path =
    case toLower << takeExtension <| path of
        ".html" ->
            "text/html"

        ".js" ->
            "text/javascript"
        
        ".css" ->
            "application/json"

        ".png" ->
            "image/png"
        
        ".jpg" ->
            "image/jpg"

        ".gif" ->
            "image/gif"

        ".wav" ->
            "audio/wav'"
        
        ".mp4" ->
            "video/mp4"
        
        ".woff" ->
            "application/font-woff"
        
        ".ttf" ->
            "application/font-ttf"
        
        ".eot" ->
            "application/vnd.ms-fontobject"

        ".otf" ->
            "application/font-otf"
        
        ".svg" ->
            "application/image/svg+xml"
        
        _  ->
            "application/octet-stream'"


-- main = run <| Native.Server.serve 8080 "localhost" urlParser typeParser
     
-- main = run <| Native.Console.println Native.Server.a
-- main = read "./public/index1.html"
--     |> map Native.Console.println
--     |> run
--
type alias Body =
    { url : String
    }
    
type Request
    = Get Body
    | Post Body
    | Put Body
    | Delete Body

type Response 
    = Response

handler : Request -> Response -> Task a ()
handler req res =
    case req of 
        Get body ->
        let 
            filePath = urlParser(body.url)
            contentType = typeParser(body.url)
            url = Native.Console.println body.url
        in
            Native.File.read filePath  (\ data -> send data res)
        _ -> succeed ()

send = Native.Server.send

main = run <| (handler |> Native.Server.http 8000 "localhost")
                    
-- main = run <| (Native.Console.println { x = 1, y= 2} )

{-
 Log data to console
-}
-- log value = Native.Console.println value

-- type Shape
--     = Rect Float Float
--     | Circle Float
--     | Triangle Float Float Float

-- space : Shape -> Float
-- space shape = 
--     case shape of 
--         Rect x y ->
--             x * y
        
--         Circle radius ->
--             radius * pi * pi
        
--         Triangle a b c ->
--             let 
--                 p = (a + b + c) /2
--             in 
--                 sqrt p * (p - a) * (p - b) * (p - c) 

-- main = run <| log << space <| Circle 2