effect module Server
    where { subscription = MySub }
    exposing
        ( send
        , listen
        , Message
        , Pack(..)
        , url
        , ReqRes
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


type alias Body =
    { url : String
    }
    
type alias ReqRes =
    { request : Body
    , response : Body
    } 

type Pack
    = Get ReqRes
    | Post ReqRes
    | Put ReqRes
    | Delete ReqRes


type alias Message =
    Result String Pack



-- type Response 
--     = Response

url : ReqRes -> String
url req =
    req.request.url

type Server
    = Server

{-| Respond to a given request
-}
send : ReqRes -> a -> Cmd msg
send =
    Native.Server.end

-- SUBSCRIPTIONS

type MySub msg
    = Listen Int (Message -> msg)


{-| Subscribe to a port
-}
listen : Int -> (Message -> msg) -> Sub msg
listen portNumber tagger =
    subscription (Listen portNumber tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        Listen portNumber tagger ->
            Listen portNumber (tagger >> func)


-- MANAGER


type alias State msg =
    { servers : Servers
    , subs : Subs msg
    }


type alias Servers =
    Dict.Dict Int Server


type alias Subs msg =
    Dict.Dict Int (List (Message -> msg))


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
    { onRequest : Pack -> Task Never ()
    , onClose : () -> Task Never ()
    }


setting : Platform.Router msg Msg -> Int -> Settings
setting router portNumber =
    { onRequest = \request -> Platform.sendToSelf router (Request portNumber request)
    , onClose = \_ -> Platform.sendToSelf router (Close portNumber)
    }
    

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
        (Listen portNumber tagger) :: tail ->
            dict
                |> Dict.update portNumber 
                    (\ list -> 
                        case list of
                            Nothing ->
                                Just [ tagger ]

                            Just list ->
                                Just (tagger :: list)
                    ) 
                |> groupSubs tail

        [] ->
            dict


-- HANDLE SELF MESSAGES


type Msg
    = Request Int Pack
    | Close Int


onSelfMsg : 
    Platform.Router msg Msg 
    -> Msg 
    -> State msg 
    -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        Request portNumber request ->
            case Dict.get portNumber state.subs of 
                Maybe.Just taggers ->
                    case taggers of
                        sub :: [] ->
                            Platform.sendToApp router (sub (Ok request))
                                |> Task.andThen (\_ -> Task.succeed state) 

                        sub :: tail ->
                            Platform.sendToApp router (sub (Ok request))
                            :: List.map (\tagger -> Platform.sendToApp router (tagger (Err "Too many subscribers"))) tail
                                |> Task.sequence
                                |> Task.andThen (\_ -> Task.succeed state) 

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