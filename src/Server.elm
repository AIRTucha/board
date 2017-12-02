module Server exposing (..)

import Native.Server
import File exposing (File)

send : File -> Response -> ()
send = Native.Server.send

http : Int -> String -> ( Request -> Response -> () ) -> ()
http = Native.Server.http

type alias Body =
    { url : String
    }
    
type Request
    = Get Body
    | Post Body
    | Put Body
    | Delete Body

type Response 
    = Response