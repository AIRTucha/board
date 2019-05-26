# WIP: Board
[![Build Status](https://travis-ci.org/AIRTucha/board.svg?branch=master)](https://travis-ci.org/AIRTucha/board)

Elm Back-end framework

# Get Started 

There are three main resources which helps to develop your first http server on Board.
 
 * The article explains motivation and main principles of the framework
 * Seed application is a project which contains a minimal possible Board Server setup
 * Demo application is a project which showcases the main features of Board framework

# Motivation

Now days almost every cloud platform offers possibilities for seamless deployment of Node.js applications. The goal of the project is to combine deployment capabilities of Node.js and safety of statically typed purely functional languages for rapid development of small micro-services. 

The main reason Elm was chosen over GHCJS or PureScript due to steeper learning curve, great documentation, active community and build-in state management system. It also has no way to be used on a back-end, so it was kinda cool to be first.

# Implementation

Board was partly inspired by Spock, Express and other Sinatra like frameworks. Typesafe URL parsing planned to be one of the primary features. The parsing engine was moved to a separate project available for everybody at Elm package manager as Pathfinder. 

## Board program

The server is composed out of three parts: 
    
* Router
* Configuration
* Binding to Node.js

which are passed to a *board* function to create an entry point for an entry point for the application. 

```elm
{-| Define server program
-}

main : Program Never () (Msg value1 () String)
main = board router config subPort
```

### Router

It is just a function which defines process of turning Request object into Response one. Request object describes an essential information about incoming inquiry like: 

* url - taken from an original Node.js Request object
* id - unique identifier which is created as hash of key parameters of the Request
* time - timestamp when the Request was initially registered in a system
* content - representation of a body for POST and PUT Requests.
* cookies - string to string dictionary which contains all cookie values associated with the request
* cargo - string to string dictionary which is used to pass information in case of multi-stage processing of the request
* ip - address of a client
* host - the host field of the request header
* protocol - identifier for an communication protocol
* method - identifier for an HTTP method

Response is a representation of the server reply. The object is matched with a client by a request id. An initial response for a Request can be created by Board.Shared.getResponse function which constructs a empty response with an id of the provided request record.

Beside id Response contains:

* content - an essential the body of reply, 
* status - an HTTP status code, 
* header - string to string dictionary which for response header values
* cookies - string to Cookie record dictionary. The record specifies cookie's properties.

#### Routing combinators

A router function is composed out of several custom request handling functions and by the routing combinators. The combinators are represented by function which takes a path description as a first argument and handler for the specified address as the second one. [Pathfinder eDSL](https://github.com/AIRTucha/pathfinder) is utilized to describe URL which is triggers the path handler. The handling function is simply responsible for turning request record and params extracted by the url parsers into one of free possible results:

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

A state management is not trivial task for a purely function application. Board utilizes Elm's architecture to create a handling tooling for accessing and modifying an application state. 

There is especial type of routing handling capable of handling to access and modify the state of the application. The access is grated by transactions. Instead of retuning the *AnswerValue* object itself such a route handler attached by such a routing combinator will return a transaction from a current state to tuple which composes state and *AnswerValue*.

```elm
{-| Path handler, query value session from local state based on cookei
-}
getSessionLocal : ( b , Request a) -> State -> ( State, AnswerValue value state error )
getSessionLocal (param, req) state =
    ( state                       # pass unmodified state forward 
    , getSession param req state  # pass param, req and state to a function which would return a reply
    )
```

The category of routing combinators includes: *useSyncState*, *getSyncState*, *postSyncState*, *putSyncState*, *deleteSyncState*, *useState*, *getState*, *postState*, *putState* and *deleteState*.

State less combinators are not capable to access current state.

```elm
{-| Path handler, query value session from db state based on cookei
-}
getSessionDB : ( b , Request a) -> Task x (AnswerValue value state error)
getSessionDB (param, req) = 
    readDict
        |> map (getSession param req)
```

Following routing combinators are included into the category: useSync, getSync, postSync, putSync, deleteSync, use, get, post, put and delete.

##### Sync and Async 

#### Initial router

##### URL parsing

#### Static server

### Config

### Node.js server

### Subscription port

## File handling

## Router 

# Known limitations

# Future plans
