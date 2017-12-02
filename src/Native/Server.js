var _airtucha$board$Native_Server = function() {
    var http = require('http');
    return {
        serve: function (port){
            return function(hostname) {
                return function (handlePath) {
                    return function (handleType) {

                        var http = require('http');
                        var fs = require('fs');
            
                        http.createServer(function (request, response) {
                            console.log( request.url );
    
                            var filePath = handlePath(request.url)
                            var contentType = handleType(filePath)

                            fs.readFile(filePath, function(error, content) {
                                if (error) {
                                    if(error.code == 'ENOENT'){
                                        fs.readFile('./404.html', function(error, content) {
                                            response.writeHead(200, { 'Content-Type': contentType });
                                            response.end(content, 'utf-8');
                                        });
                                    }
                                    else {
                                        response.writeHead(500);
                                        response.end('Sorry, check with the site admin for error: '+error.code+' ..\n');
                                        response.end();
                                    }
                                }
                                else {
                                    response.writeHead(200, { 'Content-Type': contentType });
                                    response.end(content, 'utf-8');
                                }
                            });
            
                        }).listen(port, hostname);
                    }
                }
            }
        },
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
