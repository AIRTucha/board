module File exposing (..)

import Native.File
import Future exposing( Future )

type Error 
    = Error String

type File a
    = Buffer a 

read : String -> Future File String
read path = Native.File.read path