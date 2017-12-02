var _airtucha$board$Native_File = function() {
    var scheduler = _elm_lang$core$Native_Scheduler
    var fs = require('fs');
    return {
        read: function(path) {
            return scheduler.nativeBinding(function (callback) {
                console.log("error")
                fs.readFile(path, function( error, content ) {
                    if (error) 
                        callback(scheduler.fail(error))
                    else 
                        callback(scheduler.succeed(content))
                })
            })
        }
    }
}()