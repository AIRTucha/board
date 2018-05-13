module Board.Router.ParamTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Board.Router.Param exposing (..)
import Pathfinder exposing (..)
import Shared exposing (..)


{- Test ParsingResult to Result String Board.Router.Params translation
Basic ParsingResult to Board.Router.Params converstion is straightforward,
therefore only MultiValue and Failuer is going to be tested.
-}
param : Test
param =
    describe "ParsingResult -> Params"
        [ test "Failuer to Err" <|
            \_ ->
                Failure "Some error"
                    |> parsingResult2params
                    |> Expect.equal (Err "Some error")
        , describe "MultiValue -> Params"
            [ test "single value" <|
                \_ ->
                    MultiValue [ Integer 9 ]
                        |> parsingResult2params
                        |> Expect.equal (Ok <| MultiParam [ IntParam 9 ])
            , test "two values" <|
                \_ -> 
                    MultiValue [ Integer 9, Floating 3.14 ]
                        |> parsingResult2params
                        |> Expect.equal (Ok <| MultiParam [ IntParam 9, FloatParam 3.14 ])
            , test "multiple values " <|
                \_ -> 
                    MultiValue [ Integer 9, Floating 3.14, Str "test" ]
                        |> parsingResult2params
                        |> Expect.equal (Ok <| MultiParam [ IntParam 9, FloatParam 3.14, StrParam "test" ])
            ]
        ]