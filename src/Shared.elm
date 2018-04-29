module Shared exposing (..)

import Dict exposing (Dict)
import File exposing (File, Encoding)
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
    { cookeis : Object
    , id: String
    , content : Content a
    , status : Int
    , header : Object
    }


type Protocol
    = HTTP
    | HTTPS


type alias ReqValue a b =
    { url : String
    , id : String
    , time : Int
    , content : Content a
    , cookies : b
    , cargo : Object
    , ip : String
    , host : String
    , protocol : Protocol
    }


type Request a
    = Get (ReqValue a Object)
    | Post (ReqValue a Object)
    | Put (ReqValue a Object)
    | Delete (ReqValue a Object)


response =
    { cookeis = Dict.empty
    , id = ""
    , content = Empty 
    , status = 200
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


type alias HTTPSOptions
    = Maybe  
        { key: Maybe String
        , cert: Maybe String
        , pfx: Maybe String
        , passphrase: Maybe String 
        }

