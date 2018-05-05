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


accepted = 
    Status202


badGatway = 
    Status502


badRequest =
    Status400


conflict =
    Status409


continue =
    Status100 


created =
    Status201


expectationFailed =
    Status417


failedDependency =
    Status423


faildDependency =
    Status424 


forbidden =
    Status403


gatewayTimeout =
    Status504


gone =
    Status410 


httpVersionNotSupported =
    Status505


imateapot =
    Status418
    

insufficientSpaceOnResource =
    Status419


insufficientStorage =
    Status507


internalServerError =
    Status500 


lengthRequired =
    Status411


locked =
    Status423


methodFaiure =
    Status420


methodNotAllowed = 
    Status405 


movedPemanently = 
    Status301


movedtemporarily =
    Status302 


multiStatus =
    Status207 


multipleChoice =
    Status300


networkAuthenticationRequired =
    Status511 


noContent =
    Status204


nonAuthoritativeInformation =
    Status203


notAcceptable =
    Status406


notFound =
    Status404


notImplemented =
    Status501 


notModified =
    Status304


ok = 
    Status200


partialContent =
    Status206


paymentRequired =
    Status402



parmanentRedurect =
    Status308


preconditionFailed =
    Status412 


preconditionRequired =
    Status428


processing =
    Status102



proxyAuthenticationRequired =
    Status407


requestHeaderFieldsTooLarge =
    Status431


requestTimeout =
    Status408 


requestTooLong =
    Status413 


requestUriTooLong =
    Status414 


requestedRangeNotSatisfaible =
    Status416 


resetContent =
    Status205 


seeOther =
    Status303 


serviceUnavaible =
    Status503


switchingProtocols =
    Status101 



temporaryRedirect =
    Status307


tooManyRequests =
    Status429


unauthorized =
    Status401


unprocessableEntity =
    Status422


unsupportedMediaType =
    Status415


useProxy =
    Status305


custom =
    CustomStatus
