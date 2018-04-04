effect module Server
    where { subscription = MySub }
    exposing
        ( send
        , listen
        , Message
        , url
        , RawContent(..)
        , response
        )
{-|

@docs respond, listen, Request
-}
import Dict
import Task exposing (Task)
import Native.Server
import Json.Encode as Json
import Result
import Debug 
import Dict exposing (Dict, insert)
import Bytes exposing (Bytes)
import String exposing (split)
import List exposing (foldl)
import Shared exposing (..)

type RawContent
    = Raw String String
    | UTF8 String String
    | RawJSON String
    | NoData


response =
    { cookeis = Dict.empty
    , id = ""
    , content = Empty 
    , status = 200
    , header = Dict.empty
    }




sendText str =
    { cookeis = Dict.empty
    , content = Raw str
    , status = 200
    , header = Dict.empty
    }

type alias RawRequest a =
    { url : String
    , id : String
    , time : Int
    , content : a
    , cookeis : String
    , ip : String
    , host : String
    , protocol : Protocol
    }




url req =
     case req of 
        Get val ->
            val.url

        Post val ->
            val.url

        Put val ->
            val.url

        Delete val ->
            val.url

type alias Message = (Request Content)


type Server
    = Server

{-| Respond to a given request
-}
send : Response -> ()
send res =
    case res.content of 
        File contentType data ->
            Bytes.toString data
                |> Native.Server.end res 


        JSON json ->
            Native.Server.end res json
    
        Text contentType data ->
            Native.Server.end res data

        Empty ->
             ()


-- SUBSCRIPTIONS

type MySub msg
    = Listen Int (Message -> msg) (String -> msg)


{-| Subscribe to a port
-}
listen : Int -> (Message -> msg) -> (String -> msg) -> Sub msg
listen portNumber success failuer =
    subscription (Listen portNumber success failuer)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (Listen portNumber success failuer) =
    Listen portNumber (success >> func) (failuer >> func)


-- MANAGER


type alias State msg =
    { servers : Servers
    , subs : Subs msg
    }


type alias Servers =
    Dict.Dict Int Server


type alias Subs msg =
    Dict.Dict Int (List ((Message -> msg),(String -> msg)))


init : Task Never (State msg)
init =
    Task.succeed (State Dict.empty Dict.empty)


-- HANDLE APP MESSAGES

(=>) : a -> b -> b
(=>) t1 t2 =
    (\_ -> t2) t1


onEffects :
    Platform.Router msg Msg
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router subs state =
    Task.succeed( updateSubs router (groupSubs subs Dict.empty) state.servers )
    

updateSubs
    : Platform.Router msg Msg
    -> Subs msg1
    -> Servers
    -> State msg1
updateSubs router subs servers =
    Dict.merge (addNew router) keepAll removeOld subs servers Dict.empty
        |> (\newServers -> State newServers subs)


addNew : Platform.Router msg Msg
    -> Int
    -> a
    -> Servers
    -> Servers
addNew router portNumber _ servers =
    serve router portNumber servers


serve
    : Platform.Router msg Msg
    -> Int
    -> Servers
    -> Servers
serve router portNumber servers =
    open router portNumber
        |> updateServers portNumber servers


{-| Open server which listens to a particular port.
-}
open : Platform.Router msg Msg -> Int -> Server
open router portNumber =
    Native.Server.open portNumber (setting router portNumber)


updateServers
    : comparable
    -> Dict.Dict comparable a
    -> a
    -> Dict.Dict comparable a
updateServers portNumber servers server =
    Dict.insert portNumber server servers


{-|
-}
type alias Settings =
    { onRequest :  RawRequest RawContent -> (ReqValue Content -> Request Content) -> Task Never ()
    , onClose : () -> Task Never ()
    }


setting : Platform.Router msg Msg -> Int -> Settings
setting router portNumber =
    { onRequest = \request method -> 
        request 
            |> processRequest
            |> method
            |> Input portNumber
            |> Platform.sendToSelf router
    , onClose = \_ -> Platform.sendToSelf router (Close portNumber)
    }


processRequest: RawRequest RawContent -> ReqValue Content
processRequest raw = 
    { url = raw.url
    , id = raw.id
    , time = raw.time
    , content = parseContent raw 
    , cookeis = parseCookeys raw
    , cargo = Dict.empty
    , ip = raw.ip
    , host = raw.host
    , protocol = raw.protocol
    }


parseCookeys: RawRequest RawContent -> Object 
parseCookeys req =
    req.cookeis
        |> split "; "
        |> foldl parseSinglCookey Dict.empty


parseContent: RawRequest RawContent -> Content
parseContent raw = 
    case raw.content of  
        Raw contentType data ->
            File contentType <| Bytes.fromHex data 

        UTF8 contentType data ->
            Text contentType data
        
        RawJSON data ->
            JSON data 
        
        NoData ->
            Empty


parseSinglCookey string dict =
    case split "=" string of 
        key :: value :: [] ->
            dict 
                |> insert key value
        
        _ ->
            dict

-- parseJSON  


keepAll
    : comparable
    -> a
    -> b
    -> Dict.Dict comparable b
    -> Dict.Dict comparable b
keepAll portNumber _ server servers =
    Dict.insert portNumber server servers


removeOld : Int -> Server -> Servers -> Servers
removeOld portNumber server servers =
    close server 
        => servers

{-| Close a server's connection
-}
close : Server -> ()
close =
    Native.Server.close


groupSubs : List (MySub msg) -> Subs msg -> Subs msg
groupSubs subs dict =
    case subs of
        (Listen portNumber success failuer) :: tail ->
            dict
                |> Dict.update portNumber 
                    (\ list -> 
                        case list of
                            Nothing ->
                                Just [ (success, failuer) ]

                            Just list ->
                                Just ((success, failuer) :: list)
                    ) 
                |> groupSubs tail

        [] ->
            dict


-- HANDLE SELF MESSAGES


type Msg
    = Input Int (Request Content)
    | Close Int


onSelfMsg : 
    Platform.Router msg Msg 
    -> Msg 
    -> State msg 
    -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        Input portNumber request ->
            case Dict.get portNumber state.subs of 
                Maybe.Just (taggers) ->
                    case taggers of
                        (success, failuer) :: [] ->
                            Platform.sendToApp router (success request)
                                |> Task.map (\_ -> state) 

                        (success, failuer) :: tail ->
                            Platform.sendToApp router (success request)
                            :: List.map (\tagger -> Platform.sendToApp router (failuer  "Too many subscribers")) tail
                                |> Task.sequence
                                |> Task.map (\_ -> state) 

                        _ -> 
                            Task.succeed state

                _ ->
                    Task.succeed state
                
            
        Close portNumber ->
            case Dict.get portNumber state.servers of
                Nothing ->
                    Task.succeed state

                Just _ ->
                    serve router portNumber state.servers
                        |> (\servers -> Task.succeed { state | servers = servers} )