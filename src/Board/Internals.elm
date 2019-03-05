module Board.Internals exposing (..)

{-| Collection of utility functions and types used internaly
@docs (=>)
    , (&>)
    , Mode
    , ModePacker
    , ModelToState
    , TaskModelToState
    , liftToAsync
    , AnswerValue
    , Answer
    , IncompliteStateHandler
    , StateHandler
    , Msg
-}

import Board.Shared exposing (..)
import Task exposing (Task, map)


{-| Ignor the first argument and pass the second futher
-}
(=>) : a -> b -> b
(=>) t1 t2 =
    t2


{-| Replace Task content with the second argument
-}
(&>) : Task x a -> b -> Task x b
(&>) task v =
    task
        |> map (\_ -> v) 


{-| Type for indication Sync or Async value
Async value is inclosed inside Task
-}
type Mode error value
    = Async (Task.Task error value)
    | Sync value


{-| Function which turn value to router intermidiat reply type
-}
type alias ModePacker answer value model error = 
    answer -> Mode error (Answer value model error)


{-| Function which process model an output internal reply type of router
-}
type alias ModelToState value model error =
    (model -> ( model, AnswerValue value model error ))


{-| Task which cantains function for processing of model an output internal reply type of router
-}
type alias TaskModelToState value model error =
    Task error ( ModelToState value model error )


{-| Turn Sync value to Async one
-}
liftToAsync : Mode error a -> Task.Task error a
liftToAsync value =
    case value of 
        Sync answer ->
            Task.succeed answer 
        
        Async task ->
            task


{-| Raw types of server response
-}
type AnswerValue value model error
    = Redirect String
    | Reply (Response value)
    | Next (Request value)


{-| Types of server response according to server state
-}
type Answer value model error
    = StateFull (StateHandler value model error) 
    | StateLess (AnswerValue value model error)


{-| Server response which modify or access server state with incomplite Answer type
-}
type alias IncompliteStateHandler value model error =
    model -> ( model, AnswerValue value model error )


{-| Server response which modify or access server state
-}
type alias StateHandler value model error =
    model -> (model, Mode error (Answer value model error)) 


{-| Internal msgs of server lifecircle
-}
type Msg value model error
    = Input (Request value)
    | Output (Response value)
    | Error String
    | Model (StateHandler value model error) (Request value)
    | Test String
