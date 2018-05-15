port module Port exposing (..)

import Json.Decode exposing (..)

main =
    Platform.program
        { init = Debug.log "ok" (0, Cmd.none)
        , update = update
        , subscriptions = subscriptions
        }

update message model =
    Debug.log "ok" message 
        |> \_ -> (model, Cmd.none)


type Msg
    = Input String

port suggestions : (String -> msg) -> Sub msg


subscriptions model =
    suggestions Input