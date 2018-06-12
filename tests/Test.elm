port module Test exposing (..)

import Task
import Ordeal exposing (..)

main: Ordeal
main = run emit tests

port emit : Event -> Cmd msg

tests: Test
tests =
  describe "My first suite"
    [ test "My very first test" (
      "a" |> shouldEqual "a"
    )
    , describe "A sub-suite"
      [ test "And a sub-test" (
        { a = 1, b = False } |> shouldEqual { a = 1, b = False }
      )
      , test "Another sub-test" (
        True |> shouldNotEqual False
      )
      ]
    , xtest "A skipped test" (
      "a" |> shouldEqual "b"
    )
    , test "My first async test" (
      Task.succeed 42 
        |> andTest (\result ->
            case result of 
                Ok value ->
                    value |> shouldBeGreaterThan 35
                
                Err str ->
                    failure str
        )
    )
    , test "My first failure" (
      Task.fail { a = 1, b = "aze" } 
        |> andTest (\result -> 
            case result of 
                Ok _ -> 
                    failure "some hanlder"
                
                Err {a, b} ->
                    shouldEqual "aze" b
        )
    )
    , test "Another failure" (
      ["a","b","c"] |> shouldContain "a"
    )
    ]