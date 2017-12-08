var _airtucha$board$Native_Server = function(){
    const http = require('http');
    return {
        open: function (port){
            return function(settings) {
                  return http.createServer(function (req, res) {
                    _elm_lang$core$Native_Scheduler.rawSpawn(settings.onRequest(
                        {
                            ctor: "Get",
                            _0: { 
                                request: req,
                                response: res
                            }
                        }
                    ));
                  }).on('close', function () {
                    _elm_lang$core$Native_Scheduler.rawSpawn(settings.onClose());
                  }).listen(port);
                }
        },
        end: function (request){
            return function( value ) {
                request.response.end(value);
                return { type: 'node', branches: { ctor: '[]' } }
            }
        },
        close: function (server) {
            server.close();
            return { ctor: '_Tuple0' }
        }
    };
}();

