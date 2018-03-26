module File exposing (..)

import Native.File
import Task exposing (Task)
import Bytes exposing (Bytes)

read : String -> Task Never Bytes
read path = Native.File.read path