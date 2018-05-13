module Board.Router exposing (..)

import Debug exposing (log)
import Board.Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.Router.Factory exposing (..)

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


useSyncState = router anyMethod toStateFullSync


getSyncState = router isGet toStateFullSync


postSyncState = router isPost toStateFullSync


putSyncState = router isPut toStateFullSync


deleteSyncState = router isDelete toStateFullSync


useState = router anyMethod toStateFullAsync


getState = router isGet toStateFullAsync


postState = router isPost toStateFullAsync


putState = router isPut toStateFullAsync


deleteState = router isDelete toStateFullAsync


useSync = router anyMethod stateLessSync


getSync = router isGet stateLessSync


postSync = router isPost stateLessSync


putSync = router isPut stateLessSync


deleteSync = router isDelete stateLessSync


use = router anyMethod stateLessAsync


get = router isGet stateLessAsync


post = router isPost stateLessAsync


put = router isPut stateLessAsync


delete = router isDelete stateLessAsync