port module Tests.App exposing (..)

{-| Manualy test that server boots
-}

import Pathfinder exposing (..)
import Board exposing (program)
import Board.Router exposing (..)
import Board.Shared exposing (..)
import Board.Router.Static exposing (static)
import Board.Internals exposing (..)


{-| Server configuration
-}
config : Configurations ()
config = 
    { state = ()
    , errorPrefix = Just "Warning"
    , options = 
        { portNumber = 8086
        , timeout = 1000
        , https = Nothing
        }
    }
    

{-| Define port for subscription
-}
port subPort : (String -> msg) -> Sub msg


{-| Define server program
-}
main : Program Never () (Msg String () String)
main = program router config subPort


{-| Router describes relationships between paths and request handlers
-}
router : Request String -> Mode String (Answer String () String)
router =
    -- Default router prints requests with specified prefix as default actions
    logger "Request"
        -- statically serve files from "./"
        |> static "" "./"