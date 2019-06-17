# WIP: Board
[![Build Status](https://travis-ci.org/AIRTucha/board.svg?branch=master)](https://travis-ci.org/AIRTucha/board)

An experimental typesafe back-end micro-framework for Elm.

# Get Started 

There are three main resources which helps to develop your first application based on Board:
 
 * The article explains motivation and main principles of the framework
 * Seed application is a project which contains a minimal possible Board Server setup
 * Demo application is a project which showcases the main features of Board framework

# Motivation

Now days almost every cloud platform offers possibilities for seamless deployment of Node.js applications. The goal of the project is to combine deployment capabilities of Node.js and safety of statically typed purely functional languages for rapid development of small micro-service application. 

The main reason Elm was chosen over GHCJS or PureScript due to steeper learning curve, great documentation, active community and build-in state management system. It also has no way to be used on a back-end, so it was kinda cool to be first.

# Implementation

Board was partly inspired by Spock, Express and other Sinatra like frameworks. Typesafe URL parsing planned to be one of the primary features. The parsing engine was moved to a separate project available for everybody at Elm package manager as Pathfinder. 

## Board program

The server is composed out of three parts: 
    
* Router
* Configuration
* Binding to Node.js

which are passed to a *board* function to create an entry point for an application. 

```elm
{-| Define server program
-}
main : Program Never () (Msg value1 () String)
main = board router config subPort
```

### Router

It is the fundamental part of an application, simple it is just a function which defines process of turning *Request* object into *Response* one. *Request* object describes an essential information about incoming inquiry like: 

* url - a value taken from an original Node.js Request object
* id - an unique identifier which is created as hash of key parameters of the Request
* time - a timestamp when the Request was initially registered in a system
* content - a representation of a body for POST and PUT Requests.
* cookies - a string to string dictionary which contains all cookie values associated with the request
* cargo - a string to string dictionary which is used to pass information in case of multi-stage processing of the request
* ip - an address of a client
* host - the host field of the request header
* protocol - an identifier for an communication protocol
* method - an identifier for an HTTP method

*Response* is a representation of the server reply. The object is matched with a client by a request id. An initial response for a *Request* can be created by Board.Shared.getResponse function which constructs a empty response with an id of the provided request record.

Beside id *Response* contains:

* content - essentially, the body of reply, 
* status - an HTTP status code, 
* header - string to string dictionary which for response header values
* cookies - string to Cookie record dictionary. The record specifies cookie's properties.

#### Routing combinators

A router function is composed out of several custom request handling functions and by the routing combinators. The combinators are represented by function which takes a path description as a first argument and handler for the specified address as the second one. [Pathfinder](https://github.com/AIRTucha/pathfinder) is utilized to describe URL which is triggers the path handler. The handling function is simply responsible for turning request record and params extracted by the url parsers into one of free possible results:

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

##### Stateless and State full

A state management is not trivial task for a purely function application. Board utilizes Elm's architecture to provide handy tooling for accessing and modifying an application state.

There is especial type of rout handlers capable of providing an access to the state of the application. The access is grated by transactions. Instead of retuning the *AnswerValue* object itself a route handler attached by such a routing combinator will return a transaction from a current state to tuple which composes state and *AnswerValue*.

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

Following routing combinators are included into the category: *useSync*, *getSync**, *postSync*, *putSync*, *deleteSync*, *use*, *get*, *post*, *put* and *delete*.

##### Sync and Async 

On another hand route handler can be represented by an atomic as well as an asynchronous operation. Atomic operations are usually related with some processing of request's data, parsing or local state modifications. Async ones are usually related with file handling, database manipulations or communication with third party services. An asynchronous nature of the actions is handled by Task.

Synchronous processing usually sequentiality handles the request and immediately return a correspondent response.

```elm
{-| Path handler, exclude string from path and reply that it does not exist
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

Routing combinators are responsible for combining of an existing router with new path handler. So, therefore an initial router is needed. It is represented by any function which satisfies following signature *Request String -> Mode String (Answer String State String)*. The function is going to be called once for every request. It might perfume some parsing or authentication actions. Result of the actions can be propagated by a *Cargo* property of a *Request* record.

There are two build-in initial routers: 

* *logger* prints a log message for every incoming request. The message is tagged by a prefix provided to the function as the first argument.
* *empty* just does nothing and allows a request go future.

##### URL parsing

[Pathfinder](https://github.com/AIRTucha/pathfinder) is used for URL parsing. The library provides an eDSL for URL parsing which describes expected content of URL. It has several output types which represents successful parsing of certain primitive types, multiple primitive types or failure. *Failure* indicates that the string does not match parsing specification. The types are used internally. A route handler function receives a limited version of the output with filtered out *Failure*, since it prevents the particular router handler from execution.

```elm
{-| Type which describe parsing result of URI params
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
{-| Path handler, exclude string from path and reply that it does not exist
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

Static content can easily be served with a *static* router combinator. As other ones it receives a path description as the first argument, but in this particular case the URL is simply specified by a *string* prefix to the file storage URL. The second argument represents the prefix to the files' directory on a storage.


```elm
{-| Router describes relationships between paths and request handlers
-}
router : Request String -> Mode String (Answer String State String)
router =
    -- Default router prints requests with specified prefix as default actions
    logger "Request"
        -- statically serve files from "./public/"
        |> static "" "./public/"
```

*Content-type* header is automatically generated out of a file's extension.

### Config

Configuration specifies setup of an underling Node.js HTTP/HTTPS server and initial conditions of the application.

Example of the config is presented in following listening:

```elm
{-| Server configuration
-}
config : Configurations State
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

Entire configuration descriptions is constructed out of three following types: 

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
 - *options* contains the low level properties of the server
    * *portNumber* is an integer which represent the *HTTP* port used for hosting.
    * *timeout* is an integer number which specifies an amount of milliseconds before incoming request is automatically rejected.
    * *https* is a *Maybe* monad which *Just* value contains configurations required to establish an *HTTPS* connection.

### Node.js server

Board uses calls to native Node.js API to establish the HTTP/HTTPS connection. An incoming *Request* object is processed and converted into a *Request* record exposed to Elm code. The *Response* object is placed to a *Map*. The object is popped up from the *Map* based on a *Response* record created as output of Elm code. From time to time the *Map* is cleaned out of *Responses* which are older that the *timeout*.

## File handling

File handling is implemented via a very simple library based on Node.js *fs*. Practically it contains functions to read, write and parse files. Files are represented by a higher-order function which takes a function from Node.js *Buffer* to arbitrary Elm type. The data itself is inclosed inside of closure, so that it is not directly accessible at Elm side without an appropriate handling. There are two standard parsers: *string* and *dict*. Also there are functions specialized on encoding of Elm types to File: *fromString* and *fromDict*. The last but not least there is *getContentType* function which returns *content-type* based on file name. The function powers *static*.

# Known limitations

It was an experimental project which was mainly done to investigate the possibility of adapting Elm architecture for a back-end development at the same time as improve my personal knowledge of Node.js APIs. 

Duo to the nature of Elm architecture and underlining Node.js the system in a current condition is not capable of handling multi-threaded application. Implementation of such a functionality is way beyond scope of the project right now.

The project was started right before Elm 0.19 was released. The version of Elm dramatically changed the way native code is handled. Native code is entirely forbidden for third party libraries since the release. Therefore the project didn't get any support from the main stream Elm community and it will never be available at the package manager. Also, due to dramatic changes in the infrastructure of Elm, the 0.18 and older version might be eventually discontinued.

Since it is essentially a single person pet project, there is significant lack of testing, especially production one. I will personally be happy to see project based on the library, but you have to be aware of risks.

Only HTTP and HTTPS are currently supported by the micro-framework. Sockets are out of scope. 

Some future development is required at following directions: authentications, cookies and file handling.

# Future plans

Due to recent changes in a platform, the project ended up to be just a proof of concept. Since it is not possible to update it for newer version of Elm. It is also not possible to publish it in Elm package manager, because of policy regarding native modules which are an essential part of the system in this case.

PureScript seems to be the best option for migration of the project, but lack of the Elm Architecture would require reconsideration of the state management system. At the same time it will open a opportunity to utilize and advantages of PureScript type system like Type Classes and Higher-kind types. It might be especially useful for implementation of the URL parsing eDSL.

Other viable opportunity is GHCJS, but in my point of view it is overkill, since there are many brilliant back-end frameworks for Haskell and there is no need to mess around with such a complication as translation to JS and utilization of Node.js infrastructure.
