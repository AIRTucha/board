module BoardTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Board exposing (..)
import Pathfinder exposing (..)
import Dict exposing(..)
import Board.ParamTest exposing(..)

suite : Test
suite =
    describe "Testing of Board framework modules"
        [ param
        ]