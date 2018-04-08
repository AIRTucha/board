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


write: String -> File a -> Task String ()
write path data = 
    Native.File.write path data


fromBytes: Bytes -> File a
fromBytes =
     Native.File.fromBytes


-- fromString

bytes: File a -> Bytes
bytes =
    Bytes.fromHex << string Hex 


string: Encoding -> File a -> String 
string =
    Native.File.string
