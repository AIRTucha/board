module Board.Router exposing (..)

import Debug exposing (log)
import Board.Shared exposing (..)
import Date exposing (..)
import Basics exposing (..)
import Board.Router.Internals exposing (..)
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
useSyncState = router toStateFullSync anyMethod


getSyncState 
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
getSyncState = router toStateFullSync isGet


postSyncState     
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
postSyncState = router toStateFullSync isPost


putSyncState
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
putSyncState = router toStateFullSync isPut


deleteSyncState
    : URL 
    -> PathHandler value (ModelToState value model error)
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
deleteSyncState = router toStateFullSync isDelete


useState
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
useState = router toStateFullAsync anyMethod


getState
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
getState = router toStateFullAsync isGet


postState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
postState = router toStateFullAsync isPost


putState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
putState = router toStateFullAsync isPut


deleteState 
    : URL 
    -> PathHandler value (TaskModelToState value model error1 )
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
deleteState = router toStateFullAsync isDelete


useSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
useSync = router stateLessSync anyMethod


getSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
getSync = router stateLessSync isGet


postSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
postSync = router stateLessSync isPost


putSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
putSync = router stateLessSync isPut


deleteSync 
    : URL 
    -> PathHandler value (AnswerValue value model error) 
    -> Router error request value model 
    -> request 
    -> Mode error (Answer value model error)
deleteSync = router stateLessSync isDelete


use 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
use = router stateLessAsync anyMethod


get 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
get = router stateLessAsync isGet


post 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
post = router stateLessAsync isPost


put 
    : URL 
    -> PathHandler value (Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
put = router stateLessAsync isPut


delete 
    : URL 
    -> PathHandler value (Task.Task error1 (AnswerValue value model error1)) 
    -> Router error1 request value model 
    -> request 
    -> Mode error1 (Answer value model error1)
delete = router stateLessAsync isDelete


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