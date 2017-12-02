module App exposing (..)

import Basics exposing (sqrt, pi)
import Platform exposing (program)
import Path.Generic exposing (takeExtension)
import String exposing (toLower)

import File exposing(read)
import Future exposing(apply, Future)
import Server exposing(..)
import Console exposing(..)

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
     
handler : Request -> Response -> ()
handler req res =
    case req of 
        Get body ->
            let 
                filePath = urlParser(body.url)
                    |> read
                    |> apply ( \ data -> send data res ) 
                contentType = typeParser(body.url)
                url = println body.url
            in
                ()
                    
        _ -> ()



main = run <| (handler |> http 8000 "localhost")
