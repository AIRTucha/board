module App exposing (..)

import Platform
import Server
import Task
import Console exposing(println)
import File exposing(read)
import Future exposing(apply, Future)
import Path.Generic exposing (takeExtension)
import String exposing (toLower)


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


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


type Msg
    = Request Server.Req

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Request request ->
            ( model + 1
            , 
                urlParser(Server.url request)
                    |> read
                    |> apply ( \ data -> Server.send request data ) 
                    |> (\ _ -> Cmd.none )
            )
subscriptions : Model -> Sub Msg
subscriptions model =
    Server.listen 8080 Request
    