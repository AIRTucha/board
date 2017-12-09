module File exposing (..)

import Native.File
import Task exposing (Task)

type Error 
    = Error String

type File a
    = Buffer a 

read : String -> Task Never File
read path = Native.File.read path