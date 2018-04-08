var _airtucha$board$Native_File = function() {
    var scheduler = _elm_lang$core$Native_Scheduler
    var fs = require('fs');
    return {
        read: function(path) {
            return scheduler.nativeBinding(function (callback) {
                fs.readFile(path, function( error, content ) {
                    if (error) 
                        return callback(scheduler.fail(error))
                    else 
                        return callback(scheduler.succeed( func => func(content) ))
                })
            })
        },
        write: function(path) {
            return function(file) {
                return scheduler.nativeBinding(function (callback) {
                    return file( buffer => {
                        fs.writeFile( path, buffer, function( error ) {
                            if ( error ) 
                                return callback(scheduler.fail(error))
                            else 
                                return callback(scheduler.succeed( { ctor: '_Tuple0' }))
                        })
                    })
                })
            }
        },
        string: function (encoding) {
            return function (data) {
                return data( buffer => 
                    buffer.toString( encoding.ctor.toLocaleLowerCase() )
                )
            }
        },
        fromBytes: function(str) {
            return (func) => func( new Buffer(str) )
        }
    }
}()