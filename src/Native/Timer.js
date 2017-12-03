//import Native.Scheduler //

var _airtucha$board$Native_Timer = function() {
    
    var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
    {
        callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
    });
    
    function setInterval_(interval, task)
    {
        return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
        {
            var id = setInterval(function() {
                _elm_lang$core$Native_Scheduler.rawSpawn(task);
            }, interval);
    
            return function() { clearInterval(id); };
        });
    }
    
    return {
        now: now,
        setInterval_: F2(setInterval_)
    };
    
    }();