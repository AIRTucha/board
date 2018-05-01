module App exposing (..)

import Task
import File exposing(read)
import Pathfinder exposing (..)
import Board exposing (..)
import Board.Router exposing (..)
import Shared exposing (..)
import Debug exposing (log)
config = 
    { state = 0
    , portNumber = 8081 
    , errorPrefix = Just "Warning"
    , httpsOptions = Just {
        key = Just "ok",
        cert = Nothing,
        pfx = Just "shit",
        passphrase = Nothing
    }
    }
    

main = board router config


router =
    logger "Request"
        |> useSyncState (p "/count" ) getCount
        |> useState (p "/async/count" ) getAsyncCount
        |> get (p "/") getIndex
        -- |> useSync ( p "" </> any) (\ _ -> Debug.log "Ok" Redirect "ok")
        |> static (p "") "./public/"
        -- |> get (p "/public/") getIndex
        -- |> get (p "/public") getIndex
        -- |> get (p "/public/index.html") getIndex
        -- |> get (p "/app.js") getApp
        -- |> get (p "/public/app.js") getApp
        -- |> get (p "/styles.css") getStyles
        -- |> get (p "/style.css") getStyle
        -- |> get (p "/public/styles.css") getStyles
        -- |> get (p "/public/style.css") getStyle
        -- |> use any (redirect "/")
        

getAsyncCount (param, req) =
    Task.succeed(\ model -> (model + 1, Reply <| makeTextResponse req (Basics.toString model) ))


getCount (param, req) model =
    (model + 1, Reply <| makeTextResponse req (Basics.toString model) )


getIndex =
    getFile "./public/index.html" 


getApp =
    getFile "./public/app.js" 


getStyles =
    getFile "./public/styles.css" 

getStyle =
    getFile "./public/style.css" 


redirect str _ =
    Task.succeed <| Redirect str


getFile path (param, req)  =
    path
        |> read
        |> Task.map (makeResponse req)
        |> Task.map Reply


makeResponse req file = 
    let 
        res = response
    in
        { res
        | content = Data "test" file
        , id = req.id
        } 


makeTextResponse req text = 
    let 
        res = response
    in
        { res
        | content = Text "text/plain" text
        , id = req.id
        } 