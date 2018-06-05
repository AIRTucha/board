module Board.Router exposing (..)

import Debug exposing (log)
import Board.Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.Router.Internal exposing (..)
import Board.Internals exposing (..)
import Pathfinder exposing (URL)
import Task exposing (Task)


type alias RoutHandler a b c = 
    (Params, Request a ) ->  Mode b (Answer c)


type alias ModelToState value model error =
    (model -> ( model, AnswerValue value model error ))


type alias TaskModelToState value model error =
    Task error (model -> ( model, AnswerValue value model error ))


useSyncState 
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
useSyncState = router anyMethod toStateFullSync


getSyncState 
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
getSyncState = router isGet toStateFullSync


postSyncState     
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
postSyncState = router isPost toStateFullSync


putSyncState
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
putSyncState = router isPut toStateFullSync


deleteSyncState
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
deleteSyncState = router isDelete toStateFullSync


useState
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
useState = router anyMethod toStateFullAsync


getState
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
getState = router isGet toStateFullAsync


postState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
postState = router isPost toStateFullAsync


putState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
putState = router isPut toStateFullAsync


deleteState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
deleteState = router isDelete toStateFullAsync


useSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
useSync = router anyMethod stateLessSync


getSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
getSync = router isGet stateLessSync


postSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
postSync = router isPost stateLessSync


putSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
putSync = router isPut stateLessSync


deleteSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
deleteSync = router isDelete stateLessSync


use 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
use = router anyMethod stateLessAsync


get 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
get = router isGet stateLessAsync


post 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
post = router isPost stateLessAsync


put 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
put = router isPut stateLessAsync


delete 
    : URL 
    -> PathHandler value (Task.Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
delete = router isDelete stateLessAsync


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