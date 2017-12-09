module Response exposing (..)

import Shared exposing (Object)
import Request exposing (Request)
import File exposing (File)

type Content 
    = JSON Object
    | File File
    | Raw String


type alias Body =
    { cookeis : Object
    , content : Content
    , status : Int
    , header : Object
    }


type Response 
    = Redirect String
    | Reply Body
    | Next Request