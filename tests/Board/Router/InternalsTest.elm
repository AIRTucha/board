module Board.Router.InternalsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
import Board.Router exposing (empty)
import Board.Router.Internals exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any)
import Board.Internals exposing(..)
import Tuple exposing (second)
import List exposing (concat, map)

methodCheckers = 
    [ ( isGet, "Get" )
    , ( isPost, "Post" )
    , ( isPut, "Put" )
    , ( isDelete, "Delete" )
    , ( anyMethod, "Any" )
    ]


methods = 
    [ ( Get, "Get" )
    , ( Post, "Post" )
    , ( Put, "Put" )
    , ( Delete, "Delete" )
    ]


jounMethodToChecker (checker, checkerName) (method, methodName) =
    (checker, method, "Expects " ++ checkerName ++ ", received " ++ methodName)


joinCheckerToMethods checkerTuple =
    methods
        |> map (jounMethodToChecker checkerTuple)


methodTestCases =
    methodCheckers
        |> map joinCheckerToMethods
        |> concat


testCheckerAndMethod (chekcer, method, name) =
    describe name
        [ describe "Sync Handler"
            [ describe "Sync Router" 
                [ test "Router response" <|
                    \_ -> 
                        let
                            stateLessSyncReply = Reply >> stateLessSync
                            handler = second >> Next
                            rout = getResponse >> stateLessSyncReply
                            req = getRequest Get
                            response = rout req
                        in
                            syncRouter anyMethod any handler rout req
                                |> Expect.equal response
                ]
            ]
        , describe "Sync State Router"
            [ 
                test "Pass" <|
                    \_ -> 
                        let
                            handler = \(params, req) -> Next req 
                            req = getRequest Get
                        in
                            router stateLessSync anyMethod any handler empty req
                                |> Expect.equal (nextStateLessSync req)
            ]
        -- Different handlers
            -- Different router types
                -- Check router respons
                -- Check router redirect
                -- Check router next
                    -- Check that handler redirect
                    -- Check that handler next
                    -- Check that handler respons
                    -- Check URL fails
                    -- Check URL works
        ]

{-
-}
param : Test
param =
    describe "Router Interlan logic" (map testCheckerAndMethod methodTestCases)
