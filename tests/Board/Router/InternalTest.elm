module Board.Router.InternalTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Board.Router exposing (empty)
import Board.Router.Internal exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any)
import Board.Internals exposing(..)
{-
-}
param : Test
param =
    describe "Factory"
        [ test "test" <|
            \_ ->
                let 
                    handler = (\(params, req) -> Next req)
                    req = request Get
                in
                    router anyMethod stateLessSync any handler empty req
                        |> Expect.equal (nextStateLessSync req)
        -- Check Method is correct
        -- URL match
        -- URL does not match
        -- Multiple handlers
        -- Mode is applied
        -- Async mode staies Async
        ]