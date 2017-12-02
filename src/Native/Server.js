var _airtucha$board$Native_Server = function() {
    var http = require('http');
    return {
        http: function(port) {
            return function(hostname) {
                return function(handler){
                    return http.createServer(
                        function(req, res) { 
                            handler({ ctor: 'Get', _0: req })(res)
                            // res.end()
                        }
                ).listen(port, hostname)
                }
            }
        },
        send: function(value) {
            return function(response) {
                response.end(value)
            }
        }
    }
}() 
