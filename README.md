# Board
[![Build Status](https://travis-ci.org/AIRTucha/board.svg?branch=master)](https://travis-ci.org/AIRTucha/board)

This is an experimental typesafe back-end micro-framework for Elm.

# Get Started 

There are three primary resources which help to develop your first application based on Board:
 
 * [The article](https://medium.com/@airtucha/board-an-experimental-typesafe-back-end-micro-framework-for-elm-8f193276ce36) explains motivation and main principles of the framework, which is basically a shortened version of this paper.
 * [Seed application](https://github.com/AIRTucha/board-seed) is a project which contains a minimal possible Board Server setup with all required boilerplate and deployment instructions
 * [Demo application](https://github.com/AIRTucha/board-demo) is a project which showcases the main features of Board framework

## Development instructions

The project is based on *npm* eco-system. Therefore development process is automated via *npm scripts*.

For installation of dependencies run

    npm install 

Runs minimal server to check that it kicks off

    npm start

Unit tests in a watching mode are performed by 

    npm test

A single run of test suit is dedicated to continuous integration

    npm run ci

Everybody is welcome to contribute and submit pull requests. Please communicate your ideas and suggestions via *issues*.

# Motivation

Nowadays, almost every cloud platform offers possibilities for seamless deployment of Node.js applications. The goal of the project is to combine deployment capabilities of Node.js and safety of statically typed purely functional languages for rapid development of a small micro-service application. 

The main reasons Elm was chosen over GHCJS or PureScript are a steeper learning curve, excellent documentation, active community and build-in state management system. It also has no way to be used on a back-end, so it was rather cool to be first.

# Implementation

Board was partly inspired by Spock, Express and other Sinatra like frameworks. Typesafe URL parsing planned to be one of the main features. The parsing engine was moved to a separate project available for everybody at Elm package manager as [Pathfinder](https://github.com/AIRTucha/pathfinder).

## Board program

The server is composed of three parts: 
    
* Router
* Configuration
* Binding to Node.js

They are passed to a *board* function to create an entry point for an application. 

```elm
{-| Define server program
-}
main : Program Never () (Msg value1 () String)
main = board router config subPort
```

### Router

It is the fundamental part of an application, basically it is just a function which defines the process of turning *Request* object into *Response* one. *Request* object describes essential information about incoming inquiry like: 

* url - a value taken from an original Node.js Request object
* id - a unique identifier which is created as a hash of the Request's crucial parameters
* time - a timestamp when the Request was initially registered in a system
* content - a representation of a body for POST and PUT requests.
* cookies - a string to string dictionary which contains all cookie values associated with the request
* cargo - a string to string dictionary which is used to pass information in case of multi-stage processing of the request
* ip - an address of a client
* host - the host field of the request header
* protocol - an identifier for a communication protocol
* method - an identifier for an HTTP method

*Response* is a representation of the server reply. The object is matched with a client by a request id. Board can create an initial response for a *Request*. Shared.getResponse function constructs an empty response with an id of the provided request record.

Beside id *Response* contains:

* content - essentially, the body of reply, 
* status - an HTTP status code, 
* the header - a string to string dictionary for response header values
* cookies - a string to Cookie record dictionary. The record specifies the cookie's properties.

#### Routing combinators

A router function is composed out of several custom request handling functions and by the routing combinators. The combinators are represented by a function which takes a path description as a first argument and handler for the specified address as the second one. [Pathfinder](https://github.com/AIRTucha/pathfinder) is utilized to describe the URL, which triggers the path handler. The handling function is responsible for turning the request record and params extracted by the URL parsers into one of the three possible results:

* Redirection to a new path
* Replying with an appropriate response record
* Passing the request further, possible with attached cargo

```elm
type AnswerValue value model error
    = Redirect String
    | Reply (Response value)
    | Next (Request value)
```

*use* routing combinators are capable of handling any types of HTTPS request while *get*, *post*, *put* and *delete* are only working with correspondent HTTP methods.

##### Stateless and Stateful

State management is not a trivial task for a purely functional application. Board utilizes Elm's architecture to provide handy tooling for accessing and modifying an application state.

There is a particular type of rout handlers capable of providing access to the state of the application. The access is granted by transactions. Instead of returning the *AnswerValue* record itself a route handler attached by such a routing combinator returns a transaction from a current state to tuple which composes state and *AnswerValue*.

```elm
{-| Path handler, query value session from local state based on cookie
-}
getSessionLocal : ( b , Request a) -> State -> ( State, AnswerValue value state error )
getSessionLocal (param, req) state =
    ( state                       # pass unmodified state forward 
    , getSession param req state  # pass param, req and state to a function which would return a reply
    )
```

The category of routing combinators includes: *useSyncState*, *getSyncState*, *postSyncState*, *putSyncState*, *deleteSyncState*, *useState*, *getState*, *postState*, *putState* and *deleteState*.

State-less combinators are not capable to access current state.

```elm
{-| Path handler, query value session from db state based on cookie
-}
getSessionDB : ( b , Request a) -> Task x (AnswerValue value state error)
getSessionDB (param, req) = 
    readDict
        |> map (getSession param req)
```

Following routing combinators are included into the category: *useSync*, *getSync*, *postSync*, *putSync*, *deleteSync*, *use*, *get*, *post*, *put* and *delete*.

##### Sync and Async 

On another hand, route handler can be represented by an atomic as well as an asynchronous operation. Atomic operations are usually related with some processing of request's data, parsing or local state modifications. Async ones are usually related to file handling, database manipulations or communication with third-party services. Task handles the asynchronous nature of the actions.

Synchronous processing usually sequentiality handles the request and immediately return a correspondent response.

```elm
{-| Path handler, exclude string from the path and reply that it does not exist
-}
getInvalid : ( Params, Request a ) -> AnswerValue a state error
getInvalid (param, req) =
    case param of 
        StrParam url ->
            url ++ " does not exist"
                |> makeStringResponse req 
                |> Reply
        
        _ ->
            Next req
```

Following routing combinators are included into the category: *useSync*, *getSync*, *postSync*, *putSync*, *deleteSync*, *useSyncState*, *getSyncState*, *postSyncState*, *putSyncState* and *deleteSyncState*.

Async processing is usually caused by awaiting of an asynchronous action performed based on a handled request.


```elm
{-| Path handler, update value for session at db state based on cookie
-}
postSessionDB : ( b , Request String ) -> Task String (AnswerValue a state error)
postSessionDB (param, req)  =
    readDict
        |> map (postSession param req)
        |> andThen saveSessions

```

Following routing combinators are included into the category: *useState*, *getState*, *postState*, *putState*, *deleteState*, *use*, *get*, *post*, *put* and *delete*.

#### Initial router

Routing combinators are responsible for combining of an existing router with new path handler. So, therefore, a first router is needed. It is represented by any function which satisfies following signature *Request String -> Mode String (Answer String State String)*. The function is going to be called once for every request. It might execute some parsing or authentication actions. Result of the actions can be propagated by a *Cargo* property of a *Request* record.

There are two built-in initial routers: 

* *logger* prints a log message for every incoming request. The message is tagged by a prefix provided to the function as the first argument.
* *empty* just does nothing and allows a request go future.

##### URL parsing

[Pathfinder](https://github.com/AIRTucha/pathfinder) is used for URL parsing. The library provides an eDSL for URL parsing which describes the expected content of a URL. It has several output types which represent successful parsing of certain primitive types, multiple primitive types or failure. *Failure* indicates that the string does not match parsing specification. The types are used internally. A route handler function receives a limited version of the output with filtered out *Failure* since it prevents the particular router handler from execution.

```elm
{-| Type which describes parsing the result of URI params
-}
type Params
    = IntParam Int
    | FloatParam Float
    | StrParam String
    | MultiParam (List Params)
    | QueryParam (Dict String String)
    | EmptyParam
```

The *Params* is the first element of tuple supplied to a router handler.

```elm
{-| Path handler, exclude string from the path and reply that it does not exist
-}
getInvalid : ( Params, Request a ) -> AnswerValue a state error
getInvalid (param, req) =
    case param of 
        StrParam url ->
            url ++ " does not exist"
                |> makeStringResponse req 
                |> Reply
        
        _ ->
            Next req
```

#### Static server

Static content can quickly be served with a *static* router combinator. As other ones, it receives a path description as the first argument, but in this particular case, the URL is specified by a *string* prefix to the file storage URL. The second argument represents the prefix to the files' directory on storage.

```elm
{-| Router describes relationships between paths and request handlers
-}
router: Request String -> Mode String (Answer String State String)
router =
    -- Default router prints requests with specified prefix as default actions
    logger "Request."
        -- statically serve files from "./public/"
        |> static "" "./public/"
```

*Content-type* header is automatically generated out of a file's extension.

### Config

The configuration specifies the setup of an underlying Node.js HTTP/HTTPS server and initial conditions of the application.

Example of the config is presented in the following listening:

```elm
{-| Server configuration
-}
config: Configurations State
config = 
    { state = Dict.empty
    , errorPrefix = Just "Warning"
    , options = 
        { portNumber = 8085
        , timeout = 1000
        , https = Nothing
        }
    }
```

Entire configuration descriptions are constructed out of three following types: 

```elm
{-|
-}
type alias Configurations a =
    { state: a    
    , errorPrefix: Maybe String  
    , options: Options
    }

{-|
-}
type alias Options =
    { portNumber: Int
    , timeout: Int
    , https: HTTPSOptions
    }

{-|
-}
type alias HTTPSOptions = Maybe
    { key: Maybe String
    , cert: Maybe String
    , pfx: Maybe String
    , passphrase: Maybe String 
    }
```

 - *state* represents an initial state of the applications.
 - *errorPrefix* is used for tagging of logged errors.
 - *options* contains the low-level properties of the server
    * *portNumber* is an integer which represents the *HTTP* port used for hosting.
    * *timeout* is an integer number which specifies the number of milliseconds before an incoming request is automatically rejected.
    * *https* is a *Maybe* monad which *Just* value contains configurations required to establish an *HTTPS* connection.

### Node.js server

Board uses calls to native Node.js API to establish the HTTP/HTTPS connection. An incoming *Request* object is processed and converted into a *Request* record exposed to Elm code. The *Response* object is placed to a *Map*. The object is popped up from the *Map* based on a *Response* record created as output of Elm code. From time to time the *Map* is cleaned out of *Responses* which are older than the *timeout*.

## File handling

File handling is implemented via a very simple library based on Node.js *fs*. Practically it contains functions to read, write and parse files. Files are represented by a higher-order function which takes a function from Node.js *Buffer* to arbitrary Elm type. The data itself is enclosed inside of closure so that it is not directly accessible at Elm side without proper handling. There are two standard parsers: *string* and *dict*. Also, there are functions specialized on the encoding of Elm types to File: *fromString* and *fromDict*. The last but not least there is *getContentType* function which returns *content-type* based on file name. The function powers *static*.

# Known limitations

It was an experimental project which was mainly done to investigate the possibility of adapting Elm architecture for back-end development at the same time as improving my knowledge of Node.js APIs. 

Due to the nature of Elm architecture and Node.js, the system in a current condition is not capable of handling multi-threaded application. Implementation of such a functionality is way beyond the scope of the project right now.

The project was started right before Elm 0.19 was released. The version of Elm dramatically changed the way native code is handled. Native code is entirely forbidden for third-party libraries since the release. Therefore the project didn't get any support from the mainstream Elm community, and it will never be available at the package manager. Also, due to dramatic changes in the infrastructure of Elm, the 0.18 and older version might be eventually discontinued.

Since it is essentially a single person pet project, there is a significant lack of testing, especially the production one. I will personally be happy to see the project based on the library, but you have to be aware of risks.

The micro-framework currently supports only HTTP and HTTPS. Sockets are out of scope. 

Some future development is required at following directions: an authentication, a cookies and a file handling.

# Future plans

Due to recent changes in a platform, the project ended up to be just a proof of concept. Since it is not possible to update it for the newer version of Elm, it is also not possible to publish it in Elm package manager, because of policy regarding native modules which are an essential part of the system in this case.

PureScript seems to be the best option for migration of the project, but lack of the Elm Architecture would require reconsideration of the state management system. At the same time, it will open an opportunity to utilize and advantages of PureScript type system like Type Classes and Higher-kind types. It might be especially useful for implementation of the URL parsing eDSL.

Another viable opportunity is GHCJS, but in my point of view, it is overkill since there are many brilliant back-end frameworks for Haskell and there is no need to mess around with such a complication as a translation to JS and utilization of Node.js infrastructure.
