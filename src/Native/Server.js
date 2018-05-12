const http = require('http');
const https = require('https')
const hash = require('object-hash');
const requests = new Map()
/**
 * 
 */
const emptyDict = { ctor: 'RBEmpty_elm_builtin', _0: { ctor: 'LBlack' } }
/**
 * 
 * @param {*} str 
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
 * 
 * @param {*} status 
 */
const getStatus = status => 
    status.ctor == "CustomStatus" ? status._0 : status.ctor.slice(6)
/**
 * 
 * @param {*} dict 
 * @param {*} res 
 */
const setHeaders = ( dict, res ) => {
    if(dict.ctor == 'RBNode_elm_builtin'){
        res.setHeader(dict._1, dict._2)
        return setHeaders(dict._3, setHeaders(dict._4, res))
    } else 
        return res
}
/**
 * 
 * @param {*} prefix 
 * @param {*} maybe 
 */
const maybeToString = ( prefix, maybe ) =>
    maybe.ctor == "Just" ? prefix + maybe._0 : ""
/**
 * 
 * @param {*} string 
 * @param {*} predicate 
 */
const stringIfTrue = (string, predicate) => 
    predicate = true ? string : ""
/**
 * 
 * @param {*} lifetime 
 */
const getDate = lifetime => {
    if(lifetime.ctor == "Just") {
        const date = ( new Date ).getTime() + lifetime._0
        return `Expires=${ new Date( date ).toUTCString() }`
    } else 
        return ""
}
/**
 * 
 * @param {*} name 
 * @param {*} value 
 */
const createCookie = ( name, value ) => 
    [
        `${ name }=${ value.value}`,
        getDate( value.lifetime ),
        maybeToString( "Path=", value.path ),
        maybeToString( "Domain=", value.domain ),
        stringIfTrue( "HttpOnly", value.httpOnly ),
        stringIfTrue( "Secure", value.secure )
    ].join("; ")
/**
 * 
 * @param {*} dict 
 * @param {*} array 
 */
const dictToTupleArray = ( dict, array ) => {
    if( dict.ctor == 'RBNode_elm_builtin' ){
        array.push( createCookie( dict._1, dict._2 ) )
        return dictToTupleArray( dict._3, dictToTupleArray( dict._4, array ) )
    } else 
        return array
}
/**
 * 
 * @param {*} setContent 
 */
const sendContent = setContent => contentType => getResponse => {
    const res = requests.get( response.id )
    requests.delete(response.id)
    return function( value ) {
        if( res ) {
            if( contentType )
                res.setHeader( 'Content-Type', contentType )
            res.setHeader( 'Set-Cookie',  dictToTupleArray( response.cookeis, [] ) )
            setHeaders( response.header, res )
            res.statusCode = getStatus( response.status )
            setContent( value, res );
            res.end()
        }
        return { type: 'node', branches: { ctor: '[]' } }
    }
}
/**
 * 
 * @param {*} content 
 * @param {*} contentType 
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
 * 
 * @param {*} str 
 */
const getProtocol = str => { ctor:str ? str.toUpperCase() : "HTTP" }
/**
 * 
 * @param {*} port 
 * @param {*} protocol 
 * @param {*} reqHandler 
 * @param {*} closeHandler 
 * @param {*} options 
 */
const createServer = ( port, protocol, reqHandler, closeHandler, options ) => 
    (
        options ? 
            protocol.createServer( reqHandler)  
            : 
            protocol.createServer( options, reqHandler )
    ).on( 'close', closeHandler )
     .listen( port )
/**
 * 
 */
const _airtucha$board$Native_Server = function(){
    return {
        /**
         * 
         */
        open: port => option => settings => {
            const maybeOptions = option.https
            const intervalId = setInterval(
                () => {
                    const currentTime = (new Date).getTime()
                    for( var [id, req] of requests )
                        if( req.ttl < currentTime )
                            requests.delete( id )
                },
                option.timeout
            )
            const reqHandler = function (req, res) {
                const time = (new Date).getTime()
                const id = hash( {
                    path: req.url, 
                    ip: req.connection.remoteAddress,
                    time: time,
                    ip: req.ip
                } )
                const address = req.connection.address()
                const contentType = req.headers['content-type']
                const cookies = req.headers['cookie']
                let content = undefined
                res.ttl = option.timeout + time
                requests.set( id, res )
                req
                    .on( "data", data => content = data )
                    .on( "end", () => 
                        _elm_lang$core$Native_Scheduler.rawSpawn(
                            settings.onRequest( {    
                                url : req.url,
                                id : id,
                                time : time,
                                cookies : cookies || "",
                                cargo: emptyDict,
                                content : getData( content, contentType ),
                                ip : address.address.toString(),
                                host : req.headers.host,
                                protocol : getProtocol( req.protocol ),
                                method: getMethod( req.method )
                            } )( port )
                    ))
            }
            const closeHandler = function () {
                clearInterval(intervalId)
                _elm_lang$core$Native_Scheduler.rawSpawn(settings.onClose(port)(maybeOptions));
            }
            if( maybeOptions.ctor == "Nothing" )
                return createServer( port, http, reqHandler, closeHandler )
            else {
                const options = Object.keys( maybeOptions._0 ).reduce( ( obj, key ) => {
                    const maybeValue = maybeOptions._0[key]
                    obj[key] = maybeValue.ctor == "Nothing" ? undefined : maybeValue._0
                    return obj
                }, {})
                return createServer( port, http, reqHandler, closeHandler, options )
            }   
        },
        /**
         * 
         */
        sendData: sendContent( 
            ( value, res ) => value( v => res.write( v ) ) 
        ),
        /**
         * 
         */
        sendText: contentType => 
            sendContent(
                ( value, res ) => res.write( value ) 
            )(contentType),
        /**
         * 
         */
        sendEmpty: sendContent(
            ( value, res ) => value
        )(undefined),
        /**
         * 
         */
        close: server => {
            server.close()
            return { ctor: '_Tuple0' }
        }
    }
}()

