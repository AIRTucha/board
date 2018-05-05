module Status exposing (..)
{-| HTTP status codes.

Defined by RFC1945 (HTTP/1.0), RFC2616 (HTTP/1.1), RFC2518 (WebDAV), RFC6585 (Additional HTTP Status Codes), and RFC7538 (Permanent Redirect).
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


accepted : Status
accepted = 
    Status202


badGatway : Status
badGatway = 
    Status502


badRequest : Status
badRequest =
    Status400


conflict : Status
conflict =
    Status409


continue : Status
continue =
    Status100 


created : Status
created =
    Status201


expectationFailed : Status
expectationFailed =
    Status417


failedDependency : Status
failedDependency =
    Status423


faildDependency : Status
faildDependency =
    Status424 


forbidden : Status
forbidden =
    Status403


gatewayTimeout : Status
gatewayTimeout =
    Status504


gone : Status
gone =
    Status410 


httpVersionNotSupported : Status
httpVersionNotSupported =
    Status505


imateapot : Status
imateapot =
    Status418
    

insufficientSpaceOnResource : Status
insufficientSpaceOnResource =
    Status419


insufficientStorage : Status
insufficientStorage =
    Status507


internalServerError : Status
internalServerError =
    Status500 


lengthRequired : Status
lengthRequired =
    Status411


locked : Status
locked =
    Status423


methodFaiure : Status
methodFaiure =
    Status420


methodNotAllowed : Status
methodNotAllowed = 
    Status405 


movedPemanently : Status
movedPemanently = 
    Status301


movedtemporarily : Status
movedtemporarily =
    Status302 


multiStatus : Status
multiStatus =
    Status207 


multipleChoice : Status
multipleChoice =
    Status300


networkAuthenticationRequired : Status
networkAuthenticationRequired =
    Status511 


noContent : Status
noContent =
    Status204


nonAuthoritativeInformation : Status
nonAuthoritativeInformation =
    Status203


notAcceptable : Status
notAcceptable =
    Status406


notFound : Status
notFound =
    Status404


notImplemented : Status
notImplemented =
    Status501 


notModified : Status
notModified =
    Status304


ok : Status
ok = 
    Status200


partialContent : Status
partialContent =
    Status206


paymentRequired : Status
paymentRequired =
    Status402


parmanentRedurect : Status
parmanentRedurect =
    Status308


preconditionFailed : Status
preconditionFailed =
    Status412 


preconditionRequired : Status
preconditionRequired =
    Status428


processing : Status
processing =
    Status102


proxyAuthenticationRequired : Status
proxyAuthenticationRequired =
    Status407


requestHeaderFieldsTooLarge : Status
requestHeaderFieldsTooLarge =
    Status431


requestTimeout : Status
requestTimeout =
    Status408 


requestTooLong : Status
requestTooLong =
    Status413 


requestUriTooLong : Status
requestUriTooLong =
    Status414 


requestedRangeNotSatisfaible : Status
requestedRangeNotSatisfaible =
    Status416 


resetContent : Status
resetContent =
    Status205 


seeOther : Status
seeOther =
    Status303 


serviceUnavaible : Status
serviceUnavaible =
    Status503


switchingProtocols : Status
switchingProtocols =
    Status101 


temporaryRedirect : Status
temporaryRedirect =
    Status307


tooManyRequests : Status
tooManyRequests =
    Status429


unauthorized : Status
unauthorized =
    Status401


unprocessableEntity : Status
unprocessableEntity =
    Status422


unsupportedMediaType : Status
unsupportedMediaType =
    Status415


useProxy : Status
useProxy =
    Status305


custom : Int -> Status
custom =
    CustomStatus
