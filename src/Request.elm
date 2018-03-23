module Request exposing (..)
import Shared exposing (Object)

type Protocol
    = HTTP
    | HTTPS


type alias ReqValue =
    { url : String
    , content : Object
    , cookeis : Object
    , params : Object
    , query : Object
    , cargo : Object
    , ip : String
    , host : String
    , path : String
    , protocol : Protocol
    , subdomains : List String
    , time : Int
    }


type Request
    = Get ReqValue
    | Post ReqValue
    | Put ReqValue
    | Delete ReqValue



