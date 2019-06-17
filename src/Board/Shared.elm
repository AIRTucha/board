module Board.Shared exposing 
    ( PathHandler
    , Router
    , HTTPSOptions
    , Options
    , Cookie
    , Params(..)
    , Object
    , Content(..)
    , Response
    , Protocol(..)
    , ProtoRequest
    , Request
    , Method(..)
    , Mode(..)
    , AnswerValue(..)
    , Answer(..)
    , StateHandler
    , isGet
    , isPost
    , isPut
    , isDelete
    , anyMethod
    , getResponse
    , RequestChecker
    , Configurations
    )

{-| Types and associated to them functions used acrross entier system.

@docs PathHandler
    , Router
    , HTTPSOptions
    , Options
    , Cookie
    , Params
    , Object
    , Content
    , Response
    , Protocol
    , ProtoRequest
    , Request
    , Mode
    , AnswerValue
    , Answer
    , StateHandler
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
import Task exposing (Task)


{-| Function which handles specific route
-}
type alias PathHandler value answer = 
    ( Params , Request value ) -> answer 


{-| Router defines the way request handled
-}
type alias Router error value model = 
    Request value -> Mode error (Answer value model error)

{-| Raw types of server response
-}
type AnswerValue value model error
    = Redirect String
    | Reply (Response value)
    | Next (Request value)


{-| Types of server response according to server state
-}
type Answer value model error
    = StateFull (StateHandler value model error) 
    | StateLess (AnswerValue value model error)


{-| Server response which modify or access server state
-}
type alias StateHandler value model error =
    model -> (model, Mode error (Answer value model error)) 


{-| Type for indication Sync or Async value
Async value is inclosed inside Task
-}
type Mode error value
    = Async (Task error value)
    | Sync value

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

It might carry:

* a JSON string, 
* a general purpose file with specified content type 
* a general purpose text with specified content type

-}
type Content a
    = JSON String
    | Data String (File a)
    | Text String String
    | Empty


{-| Type which represents HTTP Response 
-}
type alias Response a =
    { cookies : Dict String Cookie
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
    { cookies = Dict.empty
    , id = request.id
    , content = Empty 
    , status = notFound
    , header = Dict.empty
    }