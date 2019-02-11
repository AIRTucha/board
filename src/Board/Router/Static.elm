module Board.Router.Static exposing (static)

{-|
@docs static
-}
import Pathfinder exposing (..)
import Task exposing (Task, map, onError, succeed)
import Board.Shared exposing (..)
import Basics exposing (..)
import Board.File exposing(read, getContentType)
import Board.Status exposing (..)
import Board.Router exposing (..)
import Board.Internals exposing (..)
import Board.Router.Internals exposing (Router)


{-| Staticly serve files from specified directory for specified prefix URL
-}
static
    : URL
    -> String
    -> Router error value model
    -> Router error value model
static basePath prefix router =
    let 
        getFile (param, req) =
            let 
                next = 
                    req
                        |> Next
                        |> succeed
                onGetFileError _ = 
                    next
                res = 
                    getResponse req
                makeResponse path file = 
                    { res
                    | content = Data (getContentType path) file
                    , status = custom 200
                    }
            in
                case param of 
                    StrParam path ->
                        prefix ++ path
                            |> read
                            |> map (makeResponse path)
                            |> map Reply
                            |> onError onGetFileError
                    
                    _ ->
                        next
    in
        router
            |> get (basePath </> str) getFile



