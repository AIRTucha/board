module File exposing (..)

import Native.File
import Task exposing (Task)
import Bytes exposing (Bytes)


type Buffer = Buffer


type alias File a = (Buffer -> a) -> a


type Encoding
    = ASCII
    | UTF8
    | UTF16LE
    | Base64
    | Binary
    | Hex


read : String -> Task String (File a)
read path = 
    Native.File.read path


write: String -> File a -> Task String (File a)
write path data = 
    Native.File.write path data


fromBytes: Bytes -> File a
fromBytes =
     Bytes.toString >> fromString


fromString: String -> File a
fromString =
    Native.File.fromString

-- TODO: refactor

bytes: Buffer -> Bytes
bytes =
    Bytes.fromHex << string Hex 


string: Encoding -> Buffer -> String 
string =
    Native.File.string
