module Board.Router exposing 
    ( useSync
    , getSync
    , postSync
    , putSync
    , deleteSync
    , use
    , get
    , post
    , put
    , delete
    , useSyncState
    , getSyncState
    , postSyncState
    , putSyncState
    , deleteSyncState
    , useState
    , getState
    , postState
    , putState
    , deleteState
    , empty
    , logger
    )

{-| It is a collection of functions which are used for implementation of routing.

The router is built from prior router and routing combination function which wires rout description and handler function which process correspondent requests.

# Initial router

There are two initial options for the router.

@docs empty
    , logger

# Routing combinators

There are 20 combination functions dedicated to handling different combinations of 3 request handler properties:

- *get*, *post*, *put* and *delete* functions are used to handle requests with corresponding HTTP method. *use* one handles all method types.
- Property handler might produce an asynchronous or synchronous reply. Asynchronity is not specified since it is the primary way to handle requests related with data stored on disk, database or some third-party API.
- Managing a state at a purely functional application is hard. Therefore, the rout handler which realizes or modifies the state has to be specially indicated.

It expects a path described by Pathfinder.URL as the first argument. The second argument is the rout handler. The last one is the prior router.

Asynchronousy is handled via Task.

## Synchronous stateless router combinators

@docs useSync
    , getSync
    , postSync
    , putSync
    , deleteSync

## Asynchronous stateless router combinators
    
@docs use
    , get
    , post
    , put
    , delete

## Synchronous stateful router combinators

@docs useSyncState
    , getSyncState
    , postSyncState
    , putSyncState
    , deleteSyncState

## Asynchronous stateful router combinators

@docs useState
    , getState
    , postState
    , putState
    , deleteState
-}

import Debug exposing (log)
import Board.Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.Router.Internals exposing (..)
import Board.Internals exposing (..)
import Pathfinder exposing (URL)
import Task exposing (Task)


{-| Router combinator for synchronous handling of any request for specified rout
-}
useSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error value model 
    -> Router error value model 
useSync = syncRouter anyMethod


{-| Router combinator for synchronous handling of GET request for specified rout
-}
getSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error value model 
    -> Router error value model 
getSync = syncRouter isGet


{-| Router combinator for synchronous handling of POST request for specified rout
-}
postSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error value model 
    -> Router error value model 
postSync = syncRouter isPost


{-| Router combinator for synchronous handling of PUT request for specified rout
-}
putSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error value model 
    -> Router error value model 
putSync = syncRouter isPut


{-| Router combinator for synchronous handling of DELETE request for specified rout
-}
deleteSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error value model 
    -> Router error value model 
deleteSync = syncRouter isDelete


{-| Router combinator for asynchronous handling of any request for specified rout
-}
use 
    : URL 
    -> PathHandler value (Task error (AnswerValue value model error)) 
    -> Router error value model 
    -> Router error value model 
use = asyncRouter anyMethod


{-| Router combinator for asynchronous handling of GET request for specified rout
-}
get 
    : URL 
    -> PathHandler value (Task error (AnswerValue value model error)) 
    -> Router error value model 
    -> Router error value model 
get = asyncRouter isGet


{-| Router combinator for asynchronous handling of POST request for specified rout
-}
post 
    : URL 
    -> PathHandler value (Task error (AnswerValue value model error)) 
    -> Router error value model 
    -> Router error value model 
post = asyncRouter isPost


{-| Router combinator for asynchronous handling of PUT request for specified rout
-}
put 
    : URL 
    -> PathHandler value (Task error (AnswerValue value model error)) 
    -> Router error value model 
    -> Router error value model 
put = asyncRouter isPut


{-| Router combinator for asynchronous handling of DELETE request for specified rout
-}
delete 
    : URL 
    -> PathHandler value (Task.Task error (AnswerValue value model error)) 
    -> Router error value model 
    -> Router error value model 
delete = asyncRouter isDelete


{-| Router combinator for synchronous handling of any request for specified rout 
which involves access or modification of the server state
-}
useSyncState 
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error value model 
    -> Router error value model 
useSyncState = syncStateRouter anyMethod


{-| Router combinator for synchronous handling of GET request for specified rout 
which involves access or modification of the server state
-}
getSyncState 
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error value model 
    -> Router error value model 
getSyncState = syncStateRouter isGet


{-| Router combinator for synchronous handling of POST request for specified rout 
which involves access or modification of the server state
-}
postSyncState     
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error value model 
    -> Router error value model 
postSyncState = syncStateRouter isPost


{-| Router combinator for synchronous handling of PUT request for specified rout 
which involves access or modification of the server state
-}
putSyncState
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error value model 
    -> Router error value model 
putSyncState = syncStateRouter isPut


{-| Router combinator for synchronous handling of DELETE request for specified rout 
which involves access or modification of the server state
-}
deleteSyncState
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error value model 
    -> Router error value model 
deleteSyncState = syncStateRouter isDelete


{-| Router combinator for asynchronous handling of any request for specified rout 
which involves access or modification of the server state
-}
useState
    : URL 
    -> PathHandler value (TaskModelToState value model error )
    -> Router error value model 
    -> Router error value model 
useState = asyncStateRouter anyMethod


{-| Router combinator for asynchronous handling of GET request for specified rout 
which involves access or modification of the server state
-}
getState
    : URL 
    -> PathHandler value (TaskModelToState value model error )
    -> Router error value model 
    -> Router error value model 
getState = asyncStateRouter isGet


{-| Router combinator for asynchronous handling of POST request for specified rout 
which involves access or modification of the server state
-}
postState 
    : URL 
    -> PathHandler value (TaskModelToState value model error )
    -> Router error value model 
    -> Router error value model 
postState = asyncStateRouter isPost


{-| Router combinator for asynchronous handling of PUT request for specified rout 
which involves access or modification of the server state
-}
putState 
    : URL 
    -> PathHandler value (TaskModelToState value model error )
    -> Router error value model 
    -> Router error value model 
putState = asyncStateRouter isPut


{-| Router combinator for asynchronous handling of DELETE request for specified rout 
which involves access or modification of the server state
-}
deleteState 
    : URL 
    -> PathHandler value (TaskModelToState value model error )
    -> Router error value model 
    -> Router error value model 
deleteState = asyncStateRouter isDelete


{-| Initial router which just pass requests further
-}
empty : Request value -> Mode error1 (Answer value model error)
empty req =
    nextStateLessSync req


{-| Initial router which print a short tagged description of requests and pass them further 
-}
logger : String -> Request value -> Mode error1 (Answer value model error)
logger tag req =
    req
        |> logUrl tag
        |> nextStateLessSync


{-| Log the request with attached tag
-}
logUrl: String -> Request a -> Request a
logUrl tag req =
    Debug.log tag (reqToString req)
        => req


{-| Convert request to string
-}
reqToString: Request a -> String
reqToString req =
    case req.method of 
        Get ->
            "GET " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Post ->
            "POST " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Put ->
            "PUT " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Delete ->
            "DELETE " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)


{-| Extract and fromat date from request
-}
fromatDate : { a | time : Int } -> String
fromatDate req =    
    req.time
        |> toFloat
        |> fromTime
        |> Basics.toString