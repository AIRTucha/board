module BoardTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Board exposing (..)
import Pathfinder exposing (..)
import Dict exposing(..)
import Request exposing(..)
import Response exposing(..)

body =
    { url =  "10"
    , content = Dict.empty
    , cookeis = Dict.empty
    , params = Dict.empty
    , query = Dict.empty
    , cargo = Dict.empty
    , ip = "some ip"
    , host = "some host"
    , path = "some path"
    , protocol = HTTP
    , subdomains = ["some domain"]
    , time = 0
    }

suite : Test
suite =
    describe "Board"
        [ test "single hanlder" <|
            \_ ->
                -- handler incorrect -> go to default
                -- multiple handlers are correct
                -- multiple handler, first is not correct 
                Get body
                    |> use (int) (\ (url, req) -> Sync <| Reply <| sendText "success"  ) (\ req -> Sync <| Next req  )
                    |> Expect.equal (Sync <| Reply <| sendText "success")
        -- , test "call default" <|
        --     \_ ->
        --         Get body
        --             |> use ( int </> int </> p "ok" ) (\ (url, req) -> Finish ) (\ req -> Contenue req )
        --             |> Expect.equal (Contenue <| Get body)
        ]