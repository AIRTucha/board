module Request exposing (..)
import Dict exposing (Dict)

-- type Pack
--     = Get ReqRes
--     | Post ReqRes
--     | Put ReqRes
--     | Delete ReqRes
type Protocol
    = HTTP
    | HTTPS

type alias Body =
    { url : String
    , json : Dict String String
    , cookeis : Dict String String
    , params : Dict String String
    , query : Dict String String
    , ip : String
    , host : String
    , path : String
    , protocol : Protocol
    , subdomains : List String
    }


