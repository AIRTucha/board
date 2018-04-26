module Board exposing (..)

import Result
import Task
import Server exposing (url)
import Debug exposing (..)
import Shared exposing (..)


board router state =
    Platform.program
        { init = ( state, Cmd.none )
        , update = 
            router 
                |> server
                |> update
        , subscriptions = subscriptions
        }


-- subscriptions : Model -> Sub Msg
subscriptions model =
    Server.listen 8080 Input Error    


update server message model =
    case message of
        Input request ->
            server model request

        Output response ->
            Server.send response
                |> (\_ -> ( model, Cmd.none) )
        
        Model toState req ->
            let 
                (newModel, answer) = toState model
            in
                ( newModel
                , liftMode answer 
                    |> Task.attempt (result2output newModel req)
                )

        Error msg ->
            Debug.log msg (model, Cmd.none)


server router model req = 
    case router req of 
        Async task ->
            ( model
            , task 
                |> Task.attempt (result2output model req)
            )

        Sync value ->
            ( model
            , Task.succeed value 
                |> Task.perform (toOutput req)
            )

liftMode mode =
    case mode of 
        Sync answer ->
            Task.succeed answer 
        
        Async task ->
            task

-- result2output : Request a -> Result x (Answer a1) -> Msg
result2output model req ans =
    case ans of
        Ok value ->
            toOutput req value

        Err msg ->
            Error msg

toOutput req value =
    case value of
        StateLess v ->
            case v of
                Next newReq ->
                    Output response
                    
                Reply res ->
                    Output res 

                Redirect path ->
                    req
                        |> setURL path
                        |> Input
        
        StateFull toState ->
             Model toState req

stateResultHanler req result =
    case  result of
        Ok model2state ->
            Model model2state req 
        
        Err str ->
            Error str

replyWithState model2res req model =
    let 
        (newModel, res) = model2res model 
    in 
        ( newModel
        , res
            |> Task.succeed
            |> Task.perform Output
        )  

redirectWithState model2str req model =
    let 
        (newModel, path) = model2str model 
    in 
        ( newModel
        , setURL path req
            |> Task.succeed
            |> Task.perform Input
        )


setURL path req =
    case req of 
        Get value ->
            Get { value | url = path }

        Post value ->
            Post { value | url = path }

        Put value ->
            Put { value | url = path }

        Delete value ->
            Delete { value | url = path }