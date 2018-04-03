module File exposing (..)

import Native.File
import Task exposing (Task)
import Bytes exposing (Bytes)

read : String -> Task String Bytes
read path = 
    Native.File.read path
        |> Task.map Bytes.fromHex