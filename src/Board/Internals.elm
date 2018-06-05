module Board.Internals exposing (..)

{-|
-}

import Board.Shared exposing (..)
import Task exposing (Task)


{-|
-}
(=>) : a -> b -> b
(=>) t1 t2 =
    (\_ -> t2) t1


{-|
-}
(&>) : Task x a -> b -> Task x b
(&>) task v =
    Task.map (\_ -> v) task


{-|
-}
type Mode error value
    = Async (Task.Task error value)
    | Sync value

{-|
-}
type alias ModePacker answer value model error = 
    answer -> Mode error (Answer value model error)

{-|
-}
liftToAsync : Mode error a -> Task.Task error a
liftToAsync value =
    case value of 
        Sync answer ->
            Task.succeed answer 
        
        Async task ->
            task


{-|
-}
type AnswerValue value model error
    = Redirect String
    | Reply (Response value)
    | Next (Request value)


{-|
-}
type Answer value model error
    = StateFull (StateHandler value model error) 
    | StateLess (AnswerValue value model error)


{-|
-}
type alias StateHandler value model error =
    (model -> (model, Mode error (Answer value model error)) )


{-|
-}
type Msg value model error
    = Input (Request value)
    | Output (Response value)
    | Error String
    | Model (StateHandler value model error) (Request value)
    | Test String
