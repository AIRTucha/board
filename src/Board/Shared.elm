module Board.Shared exposing 
    ( HTTPSOptions
    , Options
    , Cookie
    , Params(..)
    , Object
    , Content(..)
    , Response
    , Protocol
    , Request
    , getRequest
    , Method(..)
    , isGet
    , isPost
    , isPut
    , isDelete
    , anyMethod
    , getResponse
    , MethodChecker
    )

{-| 
@docs HTTPSOptions
    , Options
    , Cookie
    , Params
    , Object
    , Content
    , Response
    , Protocol
    , Request
    , Method
    , isGet
    , isPost
    , isPut
    , isDelete
    , anyMethod
    , getResponse
-}

import Dict exposing (Dict)
import Board.File exposing (File, Encoding)
import Board.Status exposing (..)


{-|
-}
type alias HTTPSOptions = Maybe
    { key: Maybe String
    , cert: Maybe String
    , pfx: Maybe String
    , passphrase: Maybe String 
    }


{-|
-}
type alias Options =
    { portNumber: Int
    , timeout: Int
    , https: HTTPSOptions
    }


{-|
-}
type alias Cookie =
    { value: String
    , httpOnly: Bool
    , secure: Bool 
    , lifetime: Maybe Int
    , domain: Maybe String
    , path: Maybe String
    }


{-|
-}
type Params
    = IntParam Int
    | FloatParam Float
    | StrParam String
    | MultiParam (List Params)
    | QueryParam (Dict String String)
    | EmptyParam


{-|
-}
type alias Object =
    Dict String String


{-|
-}
type Content a
    = JSON String
    | Data String (File a)
    | Text String String
    | Empty


{-|
-}
type alias Response a =
    { cookeis : Dict String Cookie
    , id: String
    , content : Content a
    , status : Status
    , header : Object
    }


{-|
-}
type Protocol
    = HTTP
    | HTTPS


{-|
-}
type alias Request a =
    { url : String
    , id : String
    , time : Int
    , content : Content a
    , cookies : Object
    , cargo : Object
    , ip : String
    , host : String
    , protocol : Protocol
    , method: Method
    }


getRequest : Method -> Request a
getRequest method =
    { url = "example.com"
    , id = "0"
    , time = 0
    , content = Empty
    , cookies = Dict.empty
    , cargo = Dict.empty
    , ip = "0"
    , host = "example"
    , protocol = HTTP
    , method = method
    }


{-|
-}
type Method
    = Get
    | Post
    | Put
    | Delete


{-|
-}
type alias MethodChecker value = 
    Request value -> Bool


{-|
-}
isGet: Request a -> Bool
isGet req =
    req.method == Get


{-|
-}
isPost: Request a -> Bool
isPost req =
    req.method == Post


{-|
-}
isPut: Request a -> Bool
isPut req =
    req.method == Put


{-|
-}
isDelete: Request a -> Bool
isDelete req =
    req.method == Delete


{-|
-}
anyMethod: Request a -> Bool
anyMethod req =
    True


{-|
-}
getResponse: Request a -> Response a
getResponse request =
    { cookeis = Dict.empty
    , id = request.id
    , content = Empty 
    , status = notFound
    , header = Dict.empty
    }