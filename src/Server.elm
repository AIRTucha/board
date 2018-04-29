effect module Server
    where { subscription = MySub }
    exposing
        ( send
        , listen
        , Message
        , url
        , RawContent(..)
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
import File exposing (File)

type RawContent
    = Raw String String
    | UTF8 String String
    | RawJSON String
    | NoData


sendText str =
    { cookeis = Dict.empty
    , content = Raw str
    , status = 200
    , header = Dict.empty
    }

-- TODO: map
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
send : Response a -> ()
send res =
    case res.content of 
        Data contentType data ->
            Native.Server.sendData res data

        JSON json ->
            Native.Server.sendJson res json
    
        Text contentType data ->
            Native.Server.sendText contentType res data

        Empty ->
            Native.Server.sendEmpty res ()


-- SUBSCRIPTIONS

type MySub msg
    = Listen Int HTTPSOptions (Message -> msg) (String -> msg)


{-| Subscribe to a port
-}
listen portNumber options success failuer =
    subscription (Listen portNumber options success failuer)


subMap func (Listen portNumber options success failuer) =
    Listen portNumber options (success >> func) (failuer >> func)


-- MANAGER


type alias State msg =
    { servers : Servers
    , subs : Subs msg
    }


type alias Servers =
    Dict.Dict Int Server


type alias Subs msg =
    Dict.Dict Int ( List ( HTTPSOptions, (Message -> msg), (String -> msg) ) )


init =
    Task.succeed (State Dict.empty Dict.empty)


-- HANDLE APP MESSAGES
-- onEffects : Platform.Router a Msg -> List (MySub msg) -> { b | servers : Dict Int Server } -> Task x (State msg)
onEffects router subs state =
    Task.succeed( updateSubs router (groupSubs subs Dict.empty) state.servers )
    
-- updateSubs : Platform.Router a Msg -> Dict Int (List ( Message -> msg, String -> msg )) -> Dict Int Server -> State msg
updateSubs router subs servers =
    Dict.merge (addNew router) keepAll removeOld subs servers Dict.empty
        |> (\newServers -> State newServers subs)


addNew router portNumber subs servers =
    case subs of
        (options, _, _) :: _ ->
            serve router portNumber servers options

        [] ->
           servers

keepAll portNumber _ server servers =
    Dict.insert portNumber server servers


removeOld portNumber server servers =
    close server 
        => servers

-- serve : Platform.Router a Msg -> Int -> Dict Int b -> Dict Int b
serve router portNumber servers options =
    open router portNumber options
        |> updateServers portNumber servers


{-| Open server which listens to a particular port.
-}
-- open : Platform.Router a Msg -> Int -> HTTPSOptions -> b
open router portNumber option =
    Native.Server.open portNumber option (setting router portNumber option)


updateServers portNumber servers server =
    Dict.insert portNumber server servers


{-|
-}
-- type alias Settings =
--     { onRequest :  RawRequest a -> (ReqValue a -> Request a) -> Task Never ()
--     , onClose : () -> Task Never ()
--     }


-- setting : Platform.Router msg Msg -> Int -> Settings
setting router portNumber options =
    { onRequest = \request method -> 
        request 
            |> processRequest
            |> method
            |> Input portNumber
            |> Platform.sendToSelf router
    , onClose = \_ -> Platform.sendToSelf router (Close portNumber options)
    }


processRequest raw = 
    {raw | cookies = parseCookies raw}


 
parseCookies req =
    req.cookies
        |> split "; "
        |> foldl parseSinglCookie Dict.empty


parseSinglCookie string dict =
    case split "=" string of 
        key :: valueWithConf :: [] ->
            case split " " valueWithConf of 
                value :: conf ->
                    dict 
                        |> insert key value

                [] ->
                    dict 
        
        _ ->
            dict

{-| Close a server's connection
-}
close =
    Native.Server.close


groupSubs subs dict =
    case subs of
        (Listen portNumber option success failuer) :: tail ->
            dict
                |> Dict.update portNumber 
                    (\ list -> 
                        case list of
                            Nothing ->
                                Just [ (option, success, failuer) ]

                            Just list ->
                                Just ((option, success, failuer) :: list)
                    ) 
                |> groupSubs tail

        [] ->
            dict


-- HANDLE SELF MESSAGES


type Msg
    = Input Int (Request Content)
    | Close Int HTTPSOptions


onSelfMsg router selfMsg state =
    case selfMsg of
        Input portNumber request ->
            case Dict.get portNumber state.subs of 
                Maybe.Just taggers ->
                    case taggers of
                        (options, success, failuer) :: [] ->
                            Platform.sendToApp router (success request)
                                |> Task.map (\_ -> state) 

                        (options, success, failuer) :: tail ->
                            Platform.sendToApp router (success request)
                            :: List.map (\tagger -> Platform.sendToApp router (failuer  "Too many subscribers")) tail
                                |> Task.sequence
                                |> Task.map (\_ -> state) 

                        _ -> 
                            Task.succeed state

                _ ->
                    Task.succeed state
                
            
        Close portNumber options ->
            case Dict.get portNumber state.servers of
                Nothing ->
                    Task.succeed state

                Just _ ->
                    serve router portNumber state.servers options
                        |> (\servers -> Task.succeed { state | servers = servers} )