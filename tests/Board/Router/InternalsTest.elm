module Board.Router.InternalsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
import Board.Router exposing (empty)
import Board.Router.Internals exposing(..)
import Board.Shared exposing (..)
import Pathfinder exposing (any, str, int)
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


url = "test"


request method =
    let 
        req = getRequest method
    in 
        { req | url = url }


response req str = 
    let 
        res = getResponse req
    in 
        Reply { res | content = Text "text/plain" str }


next req str =
    Next { req | content = Text "text/plain" str }


getHandler hanlder (param, req) =
    case param of
        StrParam str ->
            hanlder req str
        
        _ -> 
            Next req


testCheckerAndMethod (chekcer, method, name) =
    describe name
        [ describe "Sync Handler"
            [ describe "Sync Router" 
                [ test "Router Response" <|
                    \_ -> 
                        let
                            stateLessSyncReply = Reply >> stateLessSync
                            handler = second >> Next
                            rout = getResponse >> stateLessSyncReply
                            req = getRequest method
                            response = rout req
                        in
                            syncRouter chekcer any handler rout req
                                |> Expect.equal response
                , test "Router Redirect" <|
                    \_ ->
                        let
                            stateLessSyncRedirect = Redirect >> stateLessSync
                            handler = second >> Next
                            redirect = stateLessSyncRedirect "test"
                            rout = \_ -> redirect
                            req = getRequest method
                        in
                            syncRouter chekcer any handler rout req
                                |> Expect.equal redirect
                , describe "Router Next"
                    [ test "Handler Redirect" <|
                        \_ ->
                            let
                                url = "test"
                                stateLessSyncNext = Next >> stateLessSync
                                redirect = Redirect url
                                handler = \ (param, req) ->
                                    case param of
                                        StrParam str ->
                                            Redirect str 
                                        
                                        _ -> 
                                            Next req
                                rout = stateLessSyncNext
                                rawReq = getRequest method 
                                req = { rawReq | url = url }
                                result = if chekcer req then 
                                            (stateLessSync redirect)
                                        else 
                                            stateLessSyncNext req
                            in
                                syncRouter chekcer str handler rout req
                                    |> Expect.equal result   
                    , test "Handler Reply" <|
                        \_ ->
                            let
                                stateLessSyncNext = Next >> stateLessSync
                                handler = \ (param, req ) ->
                                    case param of 
                                        StrParam str ->
                                            response req str
                                        
                                        _ ->
                                            Next req
                                rout = stateLessSyncNext
                                req = request method
                                result = if chekcer req then 
                                            (stateLessSync <| response req url)
                                        else 
                                            stateLessSyncNext req
                            in
                                syncRouter chekcer str handler rout req
                                    |> Expect.equal result  
                    , test "Handler Next" <|
                        \_ ->
                            let
                                stateLessSyncNext = Next >> stateLessSync
                                handler = getHandler next
                                rout = stateLessSyncNext
                                req = request method
                                response = Reply <| getResponse req
                                result = if chekcer req then 
                                            (stateLessSync (next req url))
                                        else 
                                            stateLessSyncNext req
                            in
                                syncRouter chekcer str handler rout req
                                    |> Expect.equal result  
                    , test "Hanler URL does not match" <|
                        \_ ->
                            let
                                stateLessSyncNext = Next >> stateLessSync
                                handler = getHandler next
                                rout = stateLessSyncNext
                                req = request method
                                response = Reply <| getResponse req
                                result = stateLessSyncNext req
                            in
                                syncRouter chekcer int handler rout req
                                    |> Expect.equal result  
                    ]
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
        ]

{-
-}
param : Test
param =
    describe "Router Interlan logic" (map testCheckerAndMethod methodTestCases)
