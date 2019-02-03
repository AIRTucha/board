module Board.Router.ParamTest exposing (..)

{- Test ParsingResult to Result String Board.Router.Params translation
Basic ParsingResult to Board.Router.Params converstion is straightforward,
therefore only MultiValue and Failuer is going to be tested.
-}

import Ordeal exposing (..)
import Board.Router.Param exposing (..)
import Pathfinder exposing (..)
import Board.Shared exposing (..)

{-| Verify Pathfinder values are correctly mapped to internal Params
-}
param : Test
param =
    describe "ParsingResult -> Params"
        [ test "Failuer to Err" 
            ( Failure "Some error"
                    |> parsingResult2params
                    |> shouldEqual (Err "Some error")
            )
        , describe "MultiValue -> Params"
            [ test "single value" 
                ( MultiValue [ Integer 9 ]
                    |> parsingResult2params
                    |> shouldEqual (Ok <| MultiParam [ IntParam 9 ])
                )
            , test "two values" 
                ( MultiValue [ Integer 9, Floating 3.14 ]
                    |> parsingResult2params
                    |> shouldEqual (Ok <| MultiParam [ IntParam 9, FloatParam 3.14 ])
                )
            , test "multiple values "
                ( MultiValue [ Integer 9, Floating 3.14, Str "test" ]
                    |> parsingResult2params
                    |> shouldEqual (Ok <| MultiParam [ IntParam 9, FloatParam 3.14, StrParam "test" ])
                )  
            ]
        ]