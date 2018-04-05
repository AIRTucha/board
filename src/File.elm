module File exposing (..)

import Native.File
import Task exposing (Task)
import Bytes exposing (Bytes)

read : String -> Task String Bytes
read path = 
    Native.File.read path
        |> Task.map Bytes.fromHex


type Data =
    Data

-- TODO: untested
write: String -> Bytes -> Task String ()
write path data = 
    data
        |> Bytes.toString
        |> Native.File.write path 