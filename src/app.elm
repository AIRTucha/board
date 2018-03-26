module App exposing (..)

import Platform
import Server exposing (ReqValue, Request(Get), Response)
import Task
import File exposing(read)
import Path.Generic exposing (takeExtension)
import String exposing (toLower)
import Debug exposing (log)
import Console exposing(println)
import Bytes exposing(Bytes)
urlParser url =
    if url == "/" then 
        "./public/index.html" 
    else 
        "./public" ++ url

main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


typeParser : String -> String
typeParser path =
    case toLower << takeExtension <| path of
        ".html" ->
            "text/html"

        ".js" ->
            "text/javascript"
        
        ".json" ->
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


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


type Msg
    = Request Server.Message
    | File (ReqValue, Bytes)
 

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Request request ->
            case request of 
                Ok req ->
                    case req of
                        Get pack ->
                            ( model + 1
                            , urlParser(Server.url pack)
                                |> read
                                |> Task.andThen (\ file -> Task.succeed(pack, file) )
                                |> Task.perform File 
                            )
                        
                        _ -> 
                            ( model, Cmd.none)

                Err msg ->
                    log msg ( model, Cmd.none)

        File (pack, file) ->
            Server.send pack file
                |> (\ _ -> ( model, Cmd.none) )
            
                    
subscriptions : Model -> Sub Msg
subscriptions model =
    Server.listen 8080 Request
    
    