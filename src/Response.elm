module Response exposing (..)

import Shared exposing (Object)
import Request exposing (Request)
import File exposing (File)
import Task exposing (..)
import Dict 

type Content 
    = JSON Object
    | File File
    | Raw String


type alias ResValue =
    { cookeis : Object
    , content : Content
    , status : Int
    , header : Object
    }


type Response
    = Redirect String
    | Reply ResValue
    | Next Request

type Mode a
    = Async (Task String a)
    | Sync a

sendText str =
    { cookeis = Dict.empty
    , content = Raw str
    , status = 200
    , header = Dict.empty
    }