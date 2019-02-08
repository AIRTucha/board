const http = require('http');
const https = require('https')
const hash = require('object-hash');
/**
 * Keep response objects for each request since they are is not exposed to Elm side
 * It is fetched from the Map during sending phase to reply back
 */ 
const responses = new Map()
/**
 * Empty Dictionary instance
 */
const emptyDict = { ctor: 'RBEmpty_elm_builtin', _0: { ctor: 'LBlack' } }
/**
 * Convert string type to HTTP method Elm value
 * @param {string} str String which represent HTTP method
 * @returns HTTP method Elm value
 */
const getMethod = str => {
    switch( str ) {
        case "GET": return { ctor: "Get" }
        case "POST": return { ctor: "Post" }
        case "PUT": return { ctor: "Put" }
        case "DELETE": return { ctor: "Delete" }
    }
}
/**
 * @param {Elm Method} status  Value which represents HTTP status
 * @returns String supported by Node.js HTTP server
 */
const getStatus = status => 
    status.ctor == "CustomStatus" ? status._0 : status.ctor.slice(6)
/**
 * @param {Elm Dict String String} dict Dictionary which contains header values by names
 * @param {JS Response} res Response associates to the headers
 * @return Response with attached headers
 */
const setHeaders = ( dict, res ) => {
    if(dict.ctor == 'RBNode_elm_builtin'){
        res.setHeader(dict._1, dict._2)
        return setHeaders(dict._3, setHeaders(dict._4, res))
    } else 
        return res
}
/**
 * @param {string} prefix String prefix prepended to the value inclosed in the Maybe
 * @param {Elm Maybe String} maybe Maybe object which keeps string
 * @returns String inclosed inside Maybe with prepended prefix or empty string
 */
const maybeToString = ( prefix, maybe ) =>
    maybe.ctor == "Just" ? prefix + maybe._0 : ""
/**
 * @param {string} string String value
 * @param {boolean} predicate Value which identifies outcome
 * @returns Based on predicate state outputs value or empty string
 */
const stringIfTrue = (string, predicate) => 
    predicate == true ? string : ""
/**
 * @param {number} lifetime Desired lifetime of cookie in ms
 * @returns Expiration indicator string for cookie
 */
const getDate = lifetime => {
    if(lifetime.ctor == "Just") {
        const date = ( new Date ).getTime() + lifetime._0
        return `expires=${ new Date( date ).toUTCString() }`
    } else 
        return ""
}
/**
 * @param {string} name Cookie name
 * @param {Elm Cookie} value Cookie description object
 * @returns Formatted cookie string
 */
const createCookie = ( name, value ) => {
    const cookie = [
        getDate( value.lifetime ),
        `${ name }=${ value.value }`,
    ]
    const path = maybeToString( "path=", value.path )
    const domain = maybeToString( "domain=", value.domain )
    const httpOnly = stringIfTrue( "HttpOnly", value.httpOnly )
    const secure = stringIfTrue( "Secure", value.secure )
    if( path )
        cookie.unshift(path)
    if( domain )
        cookie.unshift(domain)
    if( httpOnly )
        cookie.unshift(httpOnly)
    if( secure )
        cookie.unshift(secure)
    return cookie
        .reverse()
        .join("; ")
}
/**
 * @param {Elm Dict a b} dict Arbitrary dictionary
 * @param {JS Array} array Accumulator array
 * @returns Array of header strings
 */
const dictHeadersArray = ( dict, array ) => {
    if( dict.ctor == 'RBNode_elm_builtin' ){
        array.push( createCookie( dict._1, dict._2 ) )
        return dictHeadersArray( dict._3, dictHeadersArray( dict._4, array ) )
    } else 
        return array
}
/**
 * Output function that send certain type of content
 * @param {JS function} setContent Specify the way content is handled
 * @param {string} contentType MEMS string content type
 * @param {JS Response} response Node.js response object 
 * @param {Elm Data} value Data which has to be send as Response body
 * @return Elm Unit
 */
const sendContent = setContent => contentType => response => {
    const res = responses.get( response.id )
    responses.delete(response.id)
    return function( value ) {
        if( res ) {
            if( contentType )
                res.setHeader( 'Content-Type', contentType )
            res.setHeader( 'Set-Cookie',  dictHeadersArray( response.cookeis, [] ) )
            setHeaders( response.header, res )
            res.statusCode = getStatus( response.status )
            setContent( value, res );
            res.end()
        }
        return { ctor: '_Tuple0' }
    }
}
/**
 * Covert Request body to Content type according to content type
 * @param {string} content Request body
 * @param {string} contentType MEMS string content type of the Request
 * @returns Elm Content value
 */
const getData = ( content, contentType ) => {
    if( content ) {
        if ( "string" == typeof content ) {
            if( contentType == "application/json" )
                return { 
                    ctor: 'JSON',
                    _0: content
                }
            else
                return {
                    ctor: 'Text',
                    _0: contentType,
                    _1: content
                }
        } else
            return { 
                ctor: 'Data',
                _0: contentType,
                _1: func => func( content )
            }
    } else    
        return {
            ctor: 'Empty',
        }
}
/**
 * @param {string} str HTTP Protocol in string representation
 * @returns Elm Protocol value
 */
const getProtocol = str => { ctor : str === "https" ? "HTTPS" : "HTTP" }
/**
 * @param {number} port Port number to listen
 * @param {JS http_s} http_s Server library
 * @param {JS function} reqHandler Request event handler
 * @param {JS function} closeHandler Server close handler
 * @param {Elm Options} options HTTPS configuration object
 * @returns Server object
 */
const createServer = ( port, http_s, reqHandler, closeHandler, options ) => 
    (
        options ? 
            http_s.createServer( options, reqHandler )
            :
            http_s.createServer( reqHandler)  
    )
        .on( 'close', closeHandler )
        .listen( port )
/**
 * Export of native functions to Elm side
 */
const _AIRTucha$board$Native_Server = function(){
    return {
        /**
         * @param {number} port Port of HTTP server
         * @param {Elm Options} option HTTP server options
         * @param {Elm Record} handlers Collection of server event handlers
         * @return Server object
         */
        open: port => option => handlers => {
            const maybeOptions = option.https
            // Remove expired responses from collection
            const intervalId = setInterval(
                () => {
                    const currentTime = (new Date).getTime()
                    for( var [id, req] of responses )
                        if( req.ttl < currentTime )
                            responses.delete( id )
                },
                option.timeout
            )
            const reqHandler = (req, res) => {
                const time = (new Date).getTime()
                const id = hash( {
                    path: req.url, 
                    ip: req.connection.remoteAddress,
                    time: time,
                    ip: req.ip
                } )
                let content = undefined
                res.ttl = option.timeout + time
                responses.set( id, res )
                req
                    .on( "data", data => content = data )
                    .on( "end", () => 
                        _elm_lang$core$Native_Scheduler.rawSpawn(
                            handlers.onRequest( {    
                                url : req.url,
                                id : id,
                                time : time,
                                cookies : req.headers['cookie'] || "",
                                cargo: emptyDict,
                                content : getData( content, req.headers['content-type'] ),
                                ip : req.connection.address().address.toString(),
                                host : req.headers.host,
                                protocol : getProtocol( req.protocol ),
                                method: getMethod( req.method )
                            } )( port )
                    ))
            }
            const closeHandler = () => {
                clearInterval(intervalId)
                _elm_lang$core$Native_Scheduler.rawSpawn(handlers.onClose(port)(option));
            }
            return  maybeOptions.ctor === "Nothing" ?
                createServer( port, http, reqHandler, closeHandler)
            :
                createServer( 
                    port, 
                    https, 
                    reqHandler, 
                    closeHandler, 
                    Object.keys( maybeOptions ).reduce( 
                        ( obj, key ) => {
                            const maybeValue = maybeOptions[key]
                            obj[key] = maybeValue.ctor == "Nothing" ? undefined : maybeValue._0
                            return obj
                        }, 
                        {}
                    )
                )   
        },
        /**
         * Send arbitrary data
         */
        sendData: sendContent( 
            ( value, res ) => value( v => res.write( v ) ) 
        ),
        /**
         * Send text
         */
        sendText: contentType => 
            sendContent(
                ( value, res ) => res.write( value ) 
            )(contentType),
        /**
         * Send response with empty body
         */
        sendEmpty: sendContent(
            ( value, res ) => value
        )(undefined),
        /**
         * @param {JS Server} server Server to close
         * @return Elm unit
         */
        close: server => {
            server.close()
            return { ctor: '_Tuple0' }
        }
    }
}()

