module Board.Router.Static exposing (..)

{-|
@docs static
    , getFile
    , onGetFileError
    , makeResponse
-}
import Pathfinder exposing (..)
import Task
import Board.Shared exposing (..)
import Basics exposing (..)
import Board.File exposing(read, getContentType)
import Board.Status exposing (..)
import Board.Router exposing (..)
import Board.Internals exposing (..)
import Dict exposing (Dict)


{-|
-}
static 
    : URL 
    -> String 
    -> (b -> Mode error (Answer value model error)) 
    -> b 
    -> Mode error (Answer value model error)
static basePath prefix router =
    router
        |> get (basePath </> str) (getFile prefix)


{-|
-}
getFile 
    : String 
    -> ( Params, Request value ) 
    -> Task.Task x (AnswerValue value model error)
getFile prefix (param, req) =
    let 
        next = req
            |> Next
            |> Task.succeed
    in
        case param of 
            StrParam path ->
                prefix ++ path
                    |> read
                    |> Task.map (makeResponse path req)
                    |> Task.map Reply
                    |> Task.onError (onGetFileError next)
            
            _ ->
                next


{-|
-}
onGetFileError : a -> b -> a
onGetFileError value _ =
    value


{-|
-}
makeResponse 
    : String 
    -> Request a 
    -> Board.File.File a1 
    -> { cookeis : Dict.Dict String Cookie , header : Object , id : String , content : Content a1 , status : Status }
makeResponse path req file = 
    let 
        res = getResponse req
    in
        { res
        | content = Data (getContentType path) file
        , status = custom 200
        }