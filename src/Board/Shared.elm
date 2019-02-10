module Board.Shared exposing 
    ( HTTPSOptions
    , Options
    , Cookie
    , Params(..)
    , Object
    , Content(..)
    , Response
    , Protocol
    , ProtoRequest
    , Request
    , Method(..)
    , isGet
    , isPost
    , isPut
    , isDelete
    , anyMethod
    , getResponse
    , RequestChecker
    , Configurations
    )

{-| Types and associated functions used acrross entier system.

@docs HTTPSOptions
    , Options
    , Cookie
    , Params
    , Object
    , Content
    , Response
    , Protocol
    , ProtoRequest
    , Request
    , Method
    , isGet
    , isPost
    , isPut
    , isDelete
    , anyMethod
    , getResponse
    , RequestChecker
    , Configurations
-}

import Dict exposing (Dict)
import Board.File exposing (File, Encoding)
import Board.Status exposing (..)


{-| Record with configuration options for application
-}
type alias Configurations a =
    { state: a    
    , errorPrefix: Maybe String  
    , options: Options
    }


{-| Record with configuration options of HTTP protocol
-}
type alias HTTPSOptions = Maybe
    { key: Maybe String
    , cert: Maybe String
    , pfx: Maybe String
    , passphrase: Maybe String 
    }


{-| Record with configuration options of HTTP server
-}
type alias Options =
    { portNumber: Int
    , timeout: Int
    , https: HTTPSOptions
    }


{-| Record which describe single cookie value
-}
type alias Cookie =
    { value: String
    , httpOnly: Bool
    , secure: Bool 
    , lifetime: Maybe Int
    , domain: Maybe String
    , path: Maybe String
    }


{-| Type which describe parsing result of URI params
-}
type Params
    = IntParam Int
    | FloatParam Float
    | StrParam String
    | MultiParam (List Params)
    | QueryParam (Dict String String)
    | EmptyParam


{-| Record which represents simple JavaScript object with string properties
-}
type alias Object =
    Dict String String


{-| Type which represents Request or Response body
-}
type Content a
    = JSON String
    | Data String (File a)
    | Text String String
    | Empty


{-| Type which represents HTTP Response 
-}
type alias Response a =
    { cookeis : Dict String Cookie
    , id: String
    , content : Content a
    , status : Status
    , header : Object
    }


{-| Type which represent utilized protocol of the server
-}
type Protocol
    = HTTP
    | HTTPS


{-| Type which represents HTTP Request with unprocessed cookies
-}
type alias ProtoRequest a b = 
    { url : String
    , id : String
    , time : Int
    , content : Content b
    , cookies : a
    , cargo : Object
    , ip : String
    , host : String
    , protocol : Protocol
    , method: Method
    }

{-| Type which represents HTTP Request
-}
type alias Request a =
    ProtoRequest Object a


{-| Type which represents HTTP methods
-}
type Method
    = Get
    | Post
    | Put
    | Delete


{-| Type of function which checks if Request satisfy certain creteria
-}
type alias RequestChecker value = 
    Request value -> Bool


{-| Checks if Request type is GET
-}
isGet: RequestChecker a
isGet req =
    req.method == Get


{-| Checks if Request type is POST
-}
isPost: RequestChecker a
isPost req =
    req.method == Post


{-| Checks if Request type is PUT
-}
isPut: RequestChecker a
isPut req =
    req.method == Put


{-| Checks if Request type is DELETE
-}
isDelete: RequestChecker a
isDelete req =
    req.method == Delete


{-| Permites any Request type
-}
anyMethod: RequestChecker a
anyMethod req =
    True


{-| Create initial Response for Request
-}
getResponse: Request a -> Response a
getResponse request =
    { cookeis = Dict.empty
    , id = request.id
    , content = Empty 
    , status = notFound
    , header = Dict.empty
    }