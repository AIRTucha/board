effect module Server
    where { subscription = MySub }
    exposing
        ( send
        , listen
        , RawContent(..)
        )

{-| It is not correct implementation of effect module, but it is done in this way due to perfomance reasons. 
It is tightly coupled with Router and it is not going to be exposed as public API.

@docs respond, listen, Request
-}

import Dict
import Task exposing (Task)
import Native.Server
import Dict exposing (Dict, insert)
import String exposing (split)
import List exposing (foldl, map)
import Board.Shared exposing (..)
import Board.Internals exposing (..)
import Dict.Extra exposing (fromListDedupe)

{-| Types of unprocessed Req/Res body
-}
type RawContent
    = Raw String String
    | UTF8 String String
    | RawJSON String
    | NoData


{-| Ref to native server object
-}
type Server
    = Server


{-| Send Response to a given request
-}
send : Response a -> ()
send res =
    case res.content of 
        Data contentType data ->
            Native.Server.sendData contentType res data

        JSON json ->
            Native.Server.sendText "application/json" res json
    
        Text contentType data ->
            Native.Server.sendText contentType res data

        Empty ->
            Native.Server.sendEmpty res ()


{-| Format of subscription
-}
type MySub msg
    = Listener (Int, Options)


{-| Subscribe to a port
-}
listen : Options -> Sub msg 
listen options =
    subscription <| Listener (options.portNumber, options)


{-| Map over subscriptions
-}
subMap : (a -> b) -> a -> b
subMap func subs =
    func subs


{-| Collection of native servers grouped by port number
-}
type alias Servers =
    Dict.Dict Int Server


{-| State of sub. manager
-}
type alias State = 
    Servers


{-| Collection of subs
-}
type alias Subs =
    Dict.Dict Int Options


{-| Initialy collection of subs is empty
-}
init : Task x (Dict k v)
init =
    Task.succeed Dict.empty


{-| Update subs collection and manage servers accordingly
-}
onEffects : Platform.Router a Msg -> List (MySub msg) -> Dict Int Server -> Task x (Dict Int Server)
onEffects router subs servers =
    let 
        takeValue (Listener value) =
            value
        fstArg arg _ =
            arg
    in
        subs
            |> map takeValue
            |> fromListDedupe fstArg
            |> updateSubs router servers 
            |> Task.succeed
    
    
{-| Update subs collection
-}
updateSubs : Platform.Router a Msg -> Dict Int Server -> Dict Int Options -> Dict Int Server
updateSubs router servers subs =
    Dict.merge (addNew router) keepAll removeOld subs servers Dict.empty


{-| Add new sub and open server for it
-} 
addNew : Platform.Router a Msg -> Int -> Options -> Dict Int Server -> Dict Int Server
addNew router portNumber httpOptions servers =
    serve router portNumber httpOptions servers


{-| Keep all subs and servers
-}
keepAll : comparable -> a -> b -> Dict comparable b -> Dict comparable b
keepAll portNumber _ server servers =
    Dict.insert portNumber server servers


{-| Remove servers for missing subs
-}
removeOld : comparable -> Server -> Dict comparable v -> Dict comparable v
removeOld portNumber server servers =
    close server 
        => Dict.remove portNumber servers


{-| Open and add server to collection of servers
-}
serve 
    : Platform.Router a Msg 
    -> Int 
    -> Options 
    -> Dict Int Server 
    -> Dict Int Server
serve router portNumber httpOptions servers =
    Dict.insert portNumber (open router portNumber httpOptions) servers


{-| Open server which listens to a particular port.
-}
open : Platform.Router a Msg -> Int -> Options -> Server
open router portNumber option =
    let 
        onRequest request portNumber = 
            { request | cookies = parseCookies request} 
                |> OnRequest portNumber
                |> Platform.sendToSelf router
        onClose portNumber httpOptions =
            Platform.sendToSelf router <| Close portNumber httpOptions
    in
        { onRequest = onRequest
        , onClose = onClose
        }
            |> Native.Server.open portNumber option


{-| Parse cookies from string to Dict
-}
parseCookies : { a | cookies : String } -> Dict String String
parseCookies req =
    let 
        parseSingleEntry string dict =
            case split "=" string of 
                key :: value :: _ ->
                    dict 
                        |> insert key value
                
                _ ->
                    dict
    in
        req.cookies
            |> split "; "
            |> foldl parseSingleEntry Dict.empty


{-| Close a server's connection
-}
close : Server -> ()
close =
    Native.Server.close


{-| Event emitted by native server
-}
type Msg
    = OnRequest Int (Request Content)
    | Close Int Options


{-| Handler native server events
-}
onSelfMsg 
    : Platform.Router (Board.Internals.Msg Content model error) Msg 
    -> Msg 
    -> Dict Int Server 
    -> Task x (Dict Int Server)
onSelfMsg router selfMsg servers =
    case selfMsg of
        OnRequest portNumber request ->
            Platform.sendToApp router (Input request)
                &> servers
            
        Close portNumber httpOptions ->
            case Dict.get portNumber servers of
                Nothing ->
                    Task.succeed servers

                Just _ ->
                    servers
                        |> Dict.remove portNumber 
                        |> serve router portNumber httpOptions 
                        |> Task.succeed