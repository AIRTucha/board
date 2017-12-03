-- module Http exposing (..)

-- import Process
-- import Task

-- import File exposing(read)
-- import Future exposing(apply, Future)
-- import Server exposing(..)
-- import Console exposing(..)

-- {- REMOVE WHEN COMPILER BUG IS FIXED -}

-- import Json.Decode

-- main : Program Never Model Msg
-- main =
--     Platform.program
--         { init = init
--         , update = update
--         , subscriptions = subscriptions
--         }


-- type alias Model =
--     {}


-- type Msg
--     = Stop
--     | Abort


-- init : ( Model, Cmd Msg )
-- init =
--     {}
--         ! [ delayMsg 3000 <| Stop ]


-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case msg of
--         Stop ->
--             model ! [ exitApp 0 ]

--         Abort ->
--             model ! [ exitApp -1 ]


-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     externalStop <| always Abort



-- -- UTILITIES


-- delayMsg : Time -> Msg -> Cmd Msg
-- delayMsg time msg =
--     Process.sleep time
--         |> Task.perform (\_ -> msg)

module App exposing (..)

import Platform exposing (..)
import Timer  exposing (Time, second) 
import Debug exposing (log)

type alias Model = 
    { tick : Time 
    }

type Msg = Tick Time

update msg model =
    case msg of 
        Tick _ -> 
            ( 
              { model | tick = log "time" (model.tick + 1) }
            , Cmd.none
            )
    
subscriptions : Model -> Sub Msg
subscriptions model =
    Timer.every second Tick 
    
main =
    program 
    { init = (Model 0, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    }
    
