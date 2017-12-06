//import Native.Scheduler //

var _airtucha$board$Native_Timer = function() {
    var http = require('http');
    var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
    {
        callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
    });
    
    function setInterval_(interval, task)
    {
        return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
        {
            var server = http.createServer(
                function(req, res) { 
                    // handler({ ctor: 'Get', _0: req })(res)
                    _elm_lang$core$Native_Scheduler.rawSpawn(task);
                }
            ).listen(8080, 'localhost')
            return function() { server.close() };
        });
    }
    
    return {
        now: now,
        setInterval_: F2(setInterval_)
    };
    
    }();