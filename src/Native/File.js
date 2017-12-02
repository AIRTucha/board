var _airtucha$board$Native_File = function() {
    var fs = require('fs');
    return {
        read: function(path) {
            return new Promise(
                (resolve, reject) => {
                    fs.readFile(path, function( error, content ) {
                        if (error) 
                            reject(error)
                        else 
                            resolve(content)
                    })
                }
            )
        }
    }
}()