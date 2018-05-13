module Board.Shared exposing (..)

import Dict exposing (Dict)
import Board.File exposing (File, Encoding)
import Board.Status exposing (..)
import Task


type Mode error value
    = Async (Task.Task error value)
    | Sync value


type AnswerValue value model error
    = Redirect String
    | Reply (Response value)
    | Next (Request value)


type Answer value model error
    = StateFull (StateHandler value model error) 
    | StateLess (AnswerValue value model error)
   

type alias Object =
    Dict String String


type alias StateHandler value model error =
    (model -> (model, Mode error (Answer value model error)) )


type Msg value model error
    = Input (Request value)
    | Output (Response value)
    | Error String
    | Model (StateHandler value model error) (Request value)


type Content a
    = JSON String
    | Data String (File a)
    | Text String String
    | Empty


type alias Response a =
    { cookeis : Dict String Cookie
    , id: String
    , content : Content a
    , status : Status
    , header : Object
    }


type Protocol
    = HTTP
    | HTTPS


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


type Method
    = Get
    | Post
    | Put
    | Delete


getResponse: Request a -> Response a
getResponse request =
    { cookeis = Dict.empty
    , id = request.id
    , content = Empty 
    , status = notFound
    , header = Dict.empty
    }


(=>) : a -> b -> b
(=>) t1 t2 =
    (\_ -> t2) t1


(&>) task v =
    Task.map (\_ -> v) task


liftToAsync value =
    case value of 
        Sync answer ->
            Task.succeed answer 
        
        Async task ->
            task


type alias HTTPSOptions = Maybe
    { key: Maybe String
    , cert: Maybe String
    , pfx: Maybe String
    , passphrase: Maybe String 
    }


type alias Options =
    { portNumber: Int
    , timeout: Int
    , https: HTTPSOptions
    }


type alias Cookie =
    { value: String
    , httpOnly: Bool
    , secure: Bool 
    , lifetime: Maybe Int
    , domain: Maybe String
    , path: Maybe String
    }

type Params
    = IntParam Int
    | FloatParam Float
    | StrParam String
    | MultiParam (List Params)
    | QueryParam (Dict String String)
    | EmptyParam