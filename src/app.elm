module App exposing (..)

import Platform
import Server
import Task
import File exposing(read)
import Path.Generic exposing (takeExtension)
import String exposing (toLower)
import Debug exposing (log)
import Bytes exposing(Bytes)
import Pathfinder exposing (..)
import Board exposing (..)
import Board.Router exposing (..)
import Shared exposing (..)

main = board router


router =
    empty 
        |> get (p "/") getIndex
        |> get (p "/public/") getIndex
        |> get (p "/public") getIndex
        |> get (p "/public/index.html") getIndex
        |> get (p "/app.js") getApp
        |> get (p "/public/app.js") getApp
        |> get (p "/styles.css") getStyles
        |> get (p "/public/styles.css") getStyles
        |> use any (redirect "/")


getIndex =
    getFile "./public/index.html" 


getApp =
    getFile "./public/app.js" 


getStyles =
    getFile "./public/styles.css" 


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