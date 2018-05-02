effect module Server
    where { subscription = MySub }
    exposing
        ( send
        , listen
        , Message
        , RawContent(..)
        )
{-|

@docs respond, listen, Request
-}
import Dict
import Task exposing (Task)
import Native.Server

import Dict exposing (Dict, insert)
import String exposing (split)
import List exposing (foldl)
import Shared exposing (..)
import Dict.Extra exposing (fromListDedupe)

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
    = Listener (Int, HTTPSOptions)


{-| Subscribe to a port
-}
listen portNumber options =
    subscription <| Listener (portNumber, options)


subMap func (portNumber, options) =
    (portNumber, options)


-- MANAGER


type alias State = Servers


type alias Servers =
    Dict.Dict Int  Server


type alias Subs =
    Dict.Dict Int ( HTTPSOptions )

init =
    Task.succeed (Dict.empty)


-- HANDLE APP MESSAGES
onEffects router subs servers =
    subs
        |> List.map (\ (Listener v)-> v)
        |> fromListDedupe (\ first second -> first) 
        |> updateSubs router servers 
        |> Task.succeed
    

updateSubs router servers subs =
    Dict.merge (addNew router) keepAll removeOld subs servers Dict.empty


addNew router portNumber options servers =
    serve router portNumber options servers

keepAll portNumber _ server servers =
    Dict.insert portNumber server servers


removeOld portNumber server servers =
    close server 
        => Dict.remove portNumber servers


serve router portNumber options servers =
    Dict.insert portNumber (open router portNumber options) servers


{-| Open server which listens to a particular port.
-}
open router portNumber option =
    Native.Server.open portNumber option (setting router)
   

{-|
-}
-- type alias Settings =
--     { onRequest :  RawRequest a -> (Request a -> Request a) -> Task Never ()
--     , onClose : () -> Task Never ()
--     }
setting router =
    { onRequest = \request portNumber-> 
        request 
            |> processRequest
            |> OnRequest portNumber
            |> Platform.sendToSelf router
    , onClose = \portNumber options-> Platform.sendToSelf router (Close portNumber options)
    }


processRequest raw = 
    { raw | cookies = parseCookies raw} 

 
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

-- f Listener v

-- HANDLE SELF MESSAGES


type Msg
    = OnRequest Int (Request Content)
    | Close Int HTTPSOptions


onSelfMsg router selfMsg servers =
    case selfMsg of
        OnRequest portNumber request ->
            Platform.sendToApp router (Input request)
                &> servers
            
        Close portNumber options ->
            case Dict.get portNumber servers of
                Nothing ->
                    Task.succeed servers

                Just _ ->
                    servers
                        |> Dict.remove portNumber 
                        |> serve router portNumber options 
                        |> Task.succeed