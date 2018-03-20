module BoardTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Board exposing (..)
import Pathfinder exposing (..)
import Dict exposing(..)
import Request exposing(..)

body =
    { url =  "10/3.1415/ok"
    , content = Dict.empty
    , cookeis = Dict.empty
    , params = Dict.empty
    , query = Dict.empty
    , local = Dict.empty
    , ip = "some ip"
    , host = "some host"
    , path = "some path"
    , protocol = HTTP
    , subdomains = ["some domain"]
    }

suite : Test
suite =
    describe "Board"
        [ test "call hanlder" <|
            \_ ->
                Get body
                    |> use ( int </> float </> p "ok" ) (\ (url, req) -> Finish ) (\ req -> Contenue req )
                    |> Expect.equal Finish
        , test "call default" <|
            \_ ->
                Get body
                    |> use ( int </> int </> p "ok" ) (\ (url, req) -> Finish ) (\ req -> Contenue req )
                    |> Expect.equal (Contenue <| Get body)
        ]