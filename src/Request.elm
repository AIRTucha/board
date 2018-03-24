module Request exposing (..)
import Shared exposing (Object)
import Array exposing (Array)

type Protocol
    = HTTP
    | HTTPS


type alias ReqValue =
    { url : String
    , id : String
    , time : Int
    , content : Content
    -- , cookeis : Object
    , params : Object
    , query : Object
    , ip : String
    , host : String
    , protocol : Protocol
    }


type Request
    = Get ReqValue
    | Post ReqValue
    | Put ReqValue
    | Delete ReqValue

type Content 
    = JSON Object
    -- | File File
    | Raw String
    | Empty

