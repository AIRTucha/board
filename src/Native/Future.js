var _airtucha$board$Native_Future = function() {
    return {
        bind: callback => 
            promise => 
                promise
                    .then( callback ),
        catch: callback => 
            promise => 
                promise
                    .catch(callback)
    }
}()