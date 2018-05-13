module Board.Router exposing (..)

import Debug exposing (log)
import Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.RouterFactory exposing (..)

type alias RoutHandler a b c = 
    (Params, Request a ) ->  Mode b (Answer c)


type alias Router a b =
    Request a -> Mode b (Answer a)


empty req =
    nextStateLessSync req


logger tag req =
    req
        |> logUrl tag
        |> nextStateLessSync


logUrl: String -> Request a -> Request a
logUrl tag req =
    Debug.log tag (reqToMsg req)
        => req


reqToMsg: Request a -> String
reqToMsg req =
    case req.method of 
        Get ->
            "GET " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Post ->
            "POST " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Put ->
            "PUT " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)
        
        Delete ->
            "DELETE " ++ req.url ++ " ip" ++ req.ip ++ " " ++ (fromatDate req)


fromatDate req =    
    req.time
        |> toFloat
        |> fromTime
        |> Basics.toString
        

useHandler: Request a -> Bool
useHandler req =
    True


getHandler: Request a -> Bool
getHandler req =
    req.method == Get


postHandler: Request a -> Bool
postHandler req =
    req.method == Post


putHandler: Request a -> Bool
putHandler req =
    req.method == Put


deleteHandler: Request a -> Bool
deleteHandler req =
    req.method == Delete


useSyncState = router useHandler toStateFullSync


getSyncState = router getHandler toStateFullSync


postSyncState = router postHandler toStateFullSync


putSyncState = router putHandler toStateFullSync


deleteSyncState = router deleteHandler toStateFullSync


useState = router useHandler toStateFullAsync


getState = router getHandler toStateFullAsync


postState = router postHandler toStateFullAsync


putState = router putHandler toStateFullAsync


deleteState = router deleteHandler toStateFullAsync


useSync = router useHandler stateLessSync


getSync = router getHandler stateLessSync


postSync = router postHandler stateLessSync


putSync = router putHandler stateLessSync


deleteSync = router deleteHandler stateLessSync


use = router useHandler stateLessAsync


get = router getHandler stateLessAsync


post = router postHandler stateLessAsync


put = router putHandler stateLessAsync


delete = router deleteHandler stateLessAsync