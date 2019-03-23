module Board.Status exposing (..)

{-| HTTP status codes.

Defined by RFC1945 (HTTP/1.0), RFC2616 (HTTP/1.1), RFC2518 (WebDAV), RFC6585 (Additional HTTP Status Codes), and RFC7538 (Permanent Redirect).

# Status Type

Union type which defines avaible status codes

@docs Status

# Proxy shortcut

Documented functions for simplified status code selection

@docs accepted
    , badGetway 
    , badRequest 
    , conflict 
    , continue 
    , created 
    , custom 
    , expectationFailed 
    , failedDependency 
    , forbidden 
    , gatewayTimeout 
    , gone
    , httpVersionNotSupported 
    , imateapot 
    , authenticationTimeout
    , insufficientStorage 
    , internalServerError 
    , lengthRequired 
    , locked 
    , methodNotAllowed 
    , movedPermanently 
    , movedTemporarily 
    , multiStatus 
    , multipleChoice
    , networkAuthenticationRequired 
    , noContent 
    , nonAuthoritativeInformation
    , notAcceptable 
    , notFound 
    , notImplemented 
    , notModified 
    , ok 
    , parmanentRedirect
    , partialContent 
    , paymentRequired 
    , preconditionFailed 
    , preconditionRequired
    , processing 
    , proxyAuthenticationRequired 
    , requestHeaderFieldsTooLarge
    , requestTimeout 
    , payloadTooLarge 
    , requestUriTooLong 
    , requestedRangeNotSatisfaible
    , resetContent 
    , seeOther 
    , serviceUnavaible 
    , switchingProtocols 
    , temporaryRedirect 
    , tooManyRequests 
    , unauthorized 
    , unprocessableEntity 
    , unsupportedMediaType 
    , useProxy
-}

{-| Union type for avaible status codes
-}
type Status
    = Status202
    | Status502
    | Status400
    | Status409
    | Status100
    | Status201
    | Status417 
    | Status424
    | Status403
    | Status504
    | Status410
    | Status505
    | Status418
    | Status419
    | Status507
    | Status500
    | Status411
    | Status423
    | Status420
    | Status405
    | Status301
    | Status302
    | Status207
    | Status300
    | Status511
    | Status204
    | Status203
    | Status406
    | Status404
    | Status501
    | Status304
    | Status200
    | Status206
    | Status402
    | Status308
    | Status412
    | Status428
    | Status102
    | Status407
    | Status431
    | Status408
    | Status413
    | Status414
    | Status416
    | Status205
    | Status303
    | Status503
    | Status101
    | Status307
    | Status429
    | Status401
    | Status422
    | Status415
    | Status305
    | CustomStatus Int


{-| Status code 202 for indication accepted request
-}
accepted : Status
accepted = 
    Status202


{-| Status code 502 for indication of bad getway
-}
badGetway : Status
badGetway = 
    Status502


{-| Status code 400 for indication of unaccaptable request
-}
badRequest : Status
badRequest =
    Status400


{-| Status code 409 for indication of some conflict
-}
conflict : Status
conflict =
    Status409


{-| Status code 100 for indication of that reqest is valid and connection can be maintened
-}
continue : Status
continue =
    Status100 


{-| Status code 201 for indication of connection is esteblished
-}
created : Status
created =
    Status201


{-| Status code 417 for indication of request which does not satisfy expectation
-}
expectationFailed : Status
expectationFailed =
    Status417


{-| Status code 424 for indication of failure due to some unavailability of some other resource
-}
failedDependency : Status
failedDependency =
    Status424


{-| Status code 403 for indication of request which is not permited
-}
forbidden : Status
forbidden =
    Status403


{-| Status code 504 for indication of failuer due to gateway timeout
-}
gatewayTimeout : Status
gatewayTimeout =
    Status504


{-| Status code 410 for indication of resource which is avaible anymore
-}
gone : Status
gone =
    Status410 


{-| Status code 505 for indication of unsupported HTTP version
-}
httpVersionNotSupported : Status
httpVersionNotSupported =
    Status505


{-| Status code 418 is added just for fun
-}
imateapot : Status
imateapot =
    Status418
    

{-| Status code 419 for indication of authetication failure due to expired token
-}
authenticationTimeout : Status
authenticationTimeout =
    Status419


{-| Status code 507 for indication that server has insuffient storage for request handling
-}
insufficientStorage : Status
insufficientStorage =
    Status507


{-| Status code 500 for indication of unspecified internal server error
-}
internalServerError : Status
internalServerError =
    Status500 


{-| Status code 411 for indication of request failed due to unspecifed Content-Length
-}
lengthRequired : Status
lengthRequired =
    Status411


{-| Status code 423 for indication of locked resource
-}
locked : Status
locked =
    Status423


{-| Status code 405 for indication of request failed due to unsupported HTTP method
-}
methodNotAllowed : Status
methodNotAllowed = 
    Status405 


{-| Status code 301 for indication of permanently moved resources
-}
movedPermanently : Status
movedPermanently = 
    Status301


{-| Status code 302 for indication of temporarily moved resources
-}
movedTemporarily : Status
movedTemporarily =
    Status302 


{-| Status code 207 aggregate several responses to independent server requests with individual statuses
-}
multiStatus : Status
multiStatus =
    Status207 


{-| Status code 300 for indication of several avaible resources at the URI
-}
multipleChoice : Status
multipleChoice =
    Status300


{-| Status code 511 for indication authentication problems at proxy server
-}
networkAuthenticationRequired : Status
networkAuthenticationRequired =
    Status511 


{-| Status code 204 for indication of response without content
-}
noContent : Status
noContent =
    Status204


{-| Status code 203 for indication of success modified by a transforming proxy
-}
nonAuthoritativeInformation : Status
nonAuthoritativeInformation =
    Status203


{-| Status code 406 for indication that request content is not accptable
-}
notAcceptable : Status
notAcceptable =
    Status406


{-| Status code 404 for indication that requested resource is not found
-}
notFound : Status
notFound =
    Status404


{-| Status code 501 for indication of unrecogranised request, might be placeholder for future API
-}
notImplemented : Status
notImplemented =
    Status501 


{-| Status code 304 for indication that resource was not modifined since version specified in header
-}
notModified : Status
notModified =
    Status304


{-| Status code 200 for indication of succesful request
-}
ok : Status
ok = 
    Status200


{-| Status code 206 for indication that only part of requested resources is avaible
-}
partialContent : Status
partialContent =
    Status206


{-| Status code 402 for indication that resource is not avaible due to subscription limitations
-}
paymentRequired : Status
paymentRequired =
    Status402


{-| Status code 308 for indication of redirection to another URI
-}
parmanentRedirect : Status
parmanentRedirect =
    Status308


{-| Status code 412 for indication of request failed due to some prior conditions
-}
preconditionFailed : Status
preconditionFailed =
    Status412 


{-| Status code 428 for indication of request which requers some prior conditions
-}
preconditionRequired : Status
preconditionRequired =
    Status428


{-| Status code 102 for indication of time consuming request which is under processing
-}
processing : Status
processing =
    Status102


{-| Status code 407 for indication of requered proxy authentication
-}
proxyAuthenticationRequired : Status
proxyAuthenticationRequired =
    Status407


{-| Status code 407 for indication of request failed due to too large header
-}
requestHeaderFieldsTooLarge : Status
requestHeaderFieldsTooLarge =
    Status431


{-| Status code 408 for indication of request which arrived in unappropriate time
-}
requestTimeout : Status
requestTimeout =
    Status408 


{-| Status code 413 for indication of request which is to large to be handled by server
-}
payloadTooLarge : Status
payloadTooLarge =
    Status413 


{-| Status code 413 for indication of URI whihc is too long to be handled by server
-}
requestUriTooLong : Status
requestUriTooLong =
    Status414 


{-| Status code 416 for indication that it is imposible to provide requested part of file
-}
requestedRangeNotSatisfaible : Status
requestedRangeNotSatisfaible =
    Status416 


{-| Status code 205 for indication succesfully reset resource 
-}
resetContent : Status
resetContent =
    Status205 


{-| Status code 205 for indication that resoruce is avaible under another URI
-}
seeOther : Status
seeOther =
    Status303 


{-| Status code 503 for indication unavaible service
-}
serviceUnavaible : Status
serviceUnavaible =
    Status503


{-| Status code 101 for indication of succesfully switch protocol
-}
switchingProtocols : Status
switchingProtocols =
    Status101 


{-| Status code 307 for indication resource which is temporary move to different URI
-}
temporaryRedirect : Status
temporaryRedirect =
    Status307


{-| Status code 429 for indication of failure due to too many request per given time
-}
tooManyRequests : Status
tooManyRequests =
    Status429


{-| Status code 401 for indication of request failed due to authorization reasons
-}
unauthorized : Status
unauthorized =
    Status401


{-| Status code 422 for indication of well-formatted request failed due to sematic issues
-}
unprocessableEntity : Status
unprocessableEntity =
    Status422


{-| Status code 415 for indication of request which contains unsupported meadia file
-}
unsupportedMediaType : Status
unsupportedMediaType =
    Status415


{-| Status code 305 for indication of resources avaible only via proxy
-}
useProxy : Status
useProxy =
    Status305


{-| Create custom status code
-}
custom : Int -> Status
custom =
    CustomStatus