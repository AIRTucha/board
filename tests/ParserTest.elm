module ParserTest exposing (..)


import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                \_ -> Expect.equal (f "params" </> p  "values") (Collection ((Flat "params") :: (Param "values") :: []))
            ]
        ]


