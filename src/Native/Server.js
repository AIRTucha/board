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
                    // req.on("data", data => console.log(JSON.parse(data.toString('utf8')))) 
                    const address = req.connection.address()
                    const body = {    
                        url : req.url,
                        id : id,
                        time : time,
                        cookeis : emptyDict(),
                        content : empty(),
                        params : emptyDict(),
                        query : emptyDict(),
                        ip : address.address.toString(),
                        host : req.headers.host,
                        protocol : getProtocol(req.protocol)
                    }
                    requests.set( id, res )
                    _elm_lang$core$Native_Scheduler.rawSpawn(settings.onRequest(
                        {
                            ctor: getMethod(req.method),
                            _0: body
                        }
                    ));
                  }).on('close', function () {
                    _elm_lang$core$Native_Scheduler.rawSpawn(settings.onClose());
                  }).listen(port);
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

