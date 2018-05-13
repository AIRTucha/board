module App exposing (..)

import Task
import File exposing(read)
import Pathfinder exposing (..)
import Board exposing (..)
import Board.Router exposing (..)
import Shared exposing (..)
import Debug exposing (log)
import Dict exposing(insert)
import Status exposing (..)
import Board.Static exposing (..)
config = 
    { state = 0
    , errorPrefix = Just "Warning"
    , options = 
        { portNumber = 8081
        , timeout = 1000
        , https = Just 
            { key = Just "ok"
            , cert = Nothing
            , pfx = Just "shit"
            , passphrase = Nothing
            }
        }
    }
    

main = board router config


router =
    -- logger "Request"
    empty
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
        |> get (p "/styles.css") getStyle
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
        |> Task.map (makeResponse req path)
        |> Task.map Reply


makeResponse req path file = 
    let 
        res = getResponse req
    in
        { res
        | content = Data (File.getContentType path) file
        , id = req.id
        , status = ok
        , cookeis = res.cookeis
            |> insert "test1" (cookei "testvalue1") 
            -- |> insert "test2" (cookei "testvalue2") 
            -- |> insert "test3" (cookei "testvalue3") 
            -- |> insert "test4" (cookei "testvalue4") 
        } 


makeTextResponse req text = 
    let 
        res = getResponse req
    in
        { res
        | content = Text "text/plain" text
        , id = req.id
        , cookeis = res.cookeis
            |> insert "test1" (cookei "testvalue1") 
            |> insert "test2" (cookei "testvalue2") 
            |> insert "test3" (cookei "testvalue3") 
            |> insert "test4" (cookei "testvalue4") 
        } 

cookei str = 
    { value = str
    , httpOnly = True
    , secure = False 
    , lifetime = Just 1000000000
    , domain = Just "test.localhost"
    , path = Just "/count"
    }