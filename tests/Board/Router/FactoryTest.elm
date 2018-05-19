module Board.Router.FactoryTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

{-
-}
param : Test
param =
    describe "Factory"
        [ test "test" <|
            \_ ->
                Expect.equal True True
        -- Check Method is correct
        -- URL match
        -- URL does not match
        -- Multiple handlers
        -- Mode is applied
        -- Async mode staies Async
        ]