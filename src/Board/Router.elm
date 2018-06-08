module Board.Router exposing (..)

import Debug exposing (log)
import Board.Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.Router.Internals exposing (..)
import Board.Internals exposing (..)
import Pathfinder exposing (URL)
import Task exposing (Task)


useSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
useSync = syncRouter anyMethod


getSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
getSync = syncRouter isGet


postSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
postSync = syncRouter isPost


putSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
putSync = syncRouter isPut


deleteSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
deleteSync = syncRouter isDelete


use 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
use = asyncRouter anyMethod


get 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
get = asyncRouter isGet


post 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
post = asyncRouter isPost


put 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
put = asyncRouter isPut


delete 
    : URL 
    -> PathHandler value (Task.Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
delete = asyncRouter isDelete


useSyncState 
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
useSyncState = syncStateRouter anyMethod


getSyncState 
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
getSyncState = syncStateRouter isGet


postSyncState     
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
postSyncState = syncStateRouter isPost


putSyncState
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
putSyncState = syncStateRouter isPut


deleteSyncState
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
deleteSyncState = syncStateRouter isDelete


useState
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
useState = asyncStateRouter anyMethod


getState
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
getState = asyncStateRouter isGet


postState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
postState = asyncStateRouter isPost


putState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
putState = asyncStateRouter isPut


deleteState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
deleteState = asyncStateRouter isDelete


empty : Request value -> Mode error1 (Answer value model error)
empty req =
    nextStateLessSync req


logger : String -> Request value -> Mode error1 (Answer value model error)
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


fromatDate : { a | time : Int } -> String
fromatDate req =    
    req.time
        |> toFloat
        |> fromTime
        |> Basics.toString