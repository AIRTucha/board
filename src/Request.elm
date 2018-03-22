module Request exposing (..)
import Shared exposing (Object)

type Protocol
    = HTTP
    | HTTPS


type alias Body =
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
    }


type Request
    = Get Body
    | Post Body
    | Put Body
    | Delete Body



