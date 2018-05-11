const scheduler = _elm_lang$core$Native_Scheduler
const fs = require('fs');
/**
 * 
 */
const _airtucha$board$Native_File = function() {
    return {
        /**
         * 
         */
        read: path => scheduler.nativeBinding( callback => 
                fs.readFile( path, ( error, content ) =>
                    error ?
                        callback( scheduler.fail( error.toString() ) )
                    : 
                        callback( scheduler.succeed( func => func( content ) ) )
        )),
        /**
         * 
         */
        write: path => file => scheduler.nativeBinding( 
            callback =>
                file( buffer => 
                    fs.writeFile( path, buffer, error =>
                        error ?
                            callback( scheduler.fail( error ) )
                        :
                            callback( scheduler.succeed( file ) )
        ))),
        /**
         * 
         */
        string: encoding => buffer => buffer.toString( encoding.ctor.toLocaleLowerCase() ),
        /**
         * 
         */
        fromString: str => func => func( new Buffer(str) )
    }
}()