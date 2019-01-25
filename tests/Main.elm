port module Test exposing (..)

import Board.Router.ParamTest exposing (param)
import Board.Router.InternalsTest  exposing (routerInternals)
import Task
import Ordeal exposing (..)

main: Ordeal
main = run emit tests

port emit : Event -> Cmd msg

tests: Test
tests =
  describe "My first suite"
    [ param
    , routerInternals
    ]