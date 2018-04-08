var _airtucha$board$Native_Server = function(){
    const http = require('http');
    const hash = require('object-hash');
    const requests = new Map()
    const toArray = ( array ) => ({ ctor: '_Array', height: 0, table: array })
    const emptyDict = () => ({ ctor: 'RBEmpty_elm_builtin', _0: { ctor: 'LBlack' } } )
    const emptyList = () => ( { ctor: '[]' })
    const empty = () => ( {ctor: 'Empty'})
    const getMethod = (str) => {
        switch(str){
            case "GET": return "Get"
            case "POST": return "Post"
            case "PUT": return "Put"
            case "DELETE": return "Delete"
        }
    }
    const sendContent = handler => request => {
        const res = requests.get(request.id)
        requests.delete(request.id)
        return function( value ) {
            if(res)
                handler(value, res);
            return { type: 'node', branches: { ctor: '[]' } }
        }
    }
    const sendPlainText = contentType => 
        sendContent(
            ( value, res ) => res.end(value) 
        )
    const toMethod = str => body => ({
        ctor: getMethod(str),
        _0: body
    })
    const getData = (content, contentType) => {
        if(content) {
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
                    _1: func => func(content)
                }
        } else    
            return {
                ctor: 'Empty',
            }
    }
    const getProtocol = (str) => ({ctor:str ? str.toUpperCase() : "HTTP"})
    return {
        open: function (port){
            return function(settings) {
                  return http.createServer(function (req, res) {
                    const time = (new Date).getTime()
                    const id = hash( {
                        path: req.path, 
                        ip: req.connection.remoteAddress,
                        time: time
                    } )
                    const address = req.connection.address()
                    const contentType = req.headers['content-type']
                    const cookeis = req.headers['cookies']
                    let content = undefined
                    req
                        .on("data", data => {
                            content = data
                        } )
                        .on("end", () => {
                            const body = {    
                                url : req.url,
                                id : id,
                                time : time,
                                cookies : cookies || "",
                                cargo: emptyDict(),
                                content : getData(content, contentType),
                                ip : address.address.toString(),
                                host : req.headers.host,
                                protocol : getProtocol(req.protocol)
                            }
                            requests.set( id, res )
                            _elm_lang$core$Native_Scheduler.rawSpawn(
                                settings.onRequest(body)(toMethod(req.method))
                            );
                        })
                  }).on('close', function () {
                    _elm_lang$core$Native_Scheduler.rawSpawn(settings.onClose());
                  }).listen(port)
                }
        },
        sendData: sendContent( 
            ( value, res ) => value( v => res.end(v) ) 
        ),
        sendText: sendPlainText,
        sendJson: sendPlainText("application/json"),
        sendEmpty: sendContent(
            ( value, res ) => res.end() 
        ),
        close: function (server) {
            server.close();
            return { ctor: '_Tuple0' }
        }
    };
}();

