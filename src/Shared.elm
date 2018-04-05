module Shared exposing (..)

import Bytes exposing (Bytes)
import Dict exposing (Dict)

type alias Object =
    Dict String String


type Msg
    = Input (Request Content)
    | Output Response
    | Error String


type Content 
    = JSON String
    | File String Bytes
    | Text String String
    | Empty


type alias Response =
    { cookeis : Object
    , id: String
    , content : Content
    , status : Int
    , header : Object
    }


type Protocol
    = HTTP
    | HTTPS


type alias ReqValue a =
    { url : String
    , id : String
    , time : Int
    , content : a
    , cookeis : Object
    , cargo : Object
    , ip : String
    , host : String
    , protocol : Protocol
    }


type Request a
    = Get (ReqValue a)
    | Post (ReqValue a)
    | Put (ReqValue a)
    | Delete (ReqValue a)


response =
    { cookeis = Dict.empty
    , id = ""
    , content = Empty 
    , status = 200
    , header = Dict.empty
    }