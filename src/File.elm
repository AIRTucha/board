module File exposing (..)

import Native.File
import Task exposing (Task)
import Bytes exposing (Bytes)

read : String -> Task String Bytes
read path = 
    Native.File.read path
        |> Task.map bytes


write: Data a -> Bytes -> Task String ()
write path data = 
    data
        |> Bytes.toString
        |> Native.File.write path 


type Buffer = Buffer


type alias Data a = (Buffer -> a) -> a


bytes: Data a -> Bytes
bytes =
    Bytes.fromHex << string Hex 


string: Encoding -> Data a -> String 
string =
    Native.File.string


type Encoding
    = ASCII
    | UTF8
    | UTF16LE
    | Base64
    | Binary
    | Hex