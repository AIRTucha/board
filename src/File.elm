module File exposing (..)
import Native.File

type Error 
    = Error String

type File a
    = Buffer a 

-- read : String -> Task Error File
read path = Native.File.read path