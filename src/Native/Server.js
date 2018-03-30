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
    const toMethod = str => body => ({
        ctor: getMethod(str),
        _0: body
    })
    const getData = (content, contentType) => {
        if(content) {
            const splitType = contentType ? contentType.split("/") : ["", ""]
            if(splitType[0] == "text")
                return { 
                    ctor: 'UTF8',
                    _0: contentType,
                    _1: content.toString('utf8')
                }
            else if(splitType[1] == "json")
                return { 
                    ctor: 'RawJSON',
                    _0: content.toString('utf8')
                }
            else 
                return { 
                    ctor: 'Raw',
                    _0: contentType,
                    _1: content.toString('HEX')
                }
        } else    
            return {
                ctor: 'NoData',
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
                                cookeis : cookeis || "",
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
        end: function (request){
            const res = requests.get(request.id)
            requests.delete(request.id)
            return function( value ) {
                res.end(value);
                return { type: 'node', branches: { ctor: '[]' } }
            }
        },
        close: function (server) {
            server.close();
            return { ctor: '_Tuple0' }
        }
    };
}();

