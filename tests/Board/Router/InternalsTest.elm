module Board.Router.InternalsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Board.Router exposing (empty)
import Board.Router.Internals exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any)
import Board.Internals exposing(..)
{-
-}
param : Test
param =
    describe "Router Interlan logic"
        [ describe "Sync State Router"
            [ 
                test "Pass" <|
                    \_ -> 
                        let
                            handler = \(params, req) -> Next req 
                            req = request Get
                        in
                            router stateLessSync anyMethod any handler empty req
                                |> Expect.equal (nextStateLessSync req)
            ]
        , describe "Sync Router"
            [ 
                test "place holder 1" <|
                    \_ -> 
                        Expect.equal True True
            ],
            test "place holder 2" <|
                \_ ->
                    let
                        handler = (\(params, req) -> Next req)
                        req = request Get
                    in
                        router stateLessSync anyMethod any handler empty req
                            |> Expect.equal (nextStateLessSync req)
        -- Different handlers
            -- Different router types
                -- Check router redirect
                -- Check router next
                -- Check router respons
                -- Check that handler redirect
                -- Check that handler next
                -- Check that handler respons
                -- Check Method fails
                -- Check Method works
                -- Check URL fails
                -- Check URL works
        -- Multiple handlers
        ]