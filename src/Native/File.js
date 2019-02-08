const scheduler = _elm_lang$core$Native_Scheduler
const fs = require('fs')
/**
 * @param {string} path String to be adjusted
 * @returns String with removed './' from start if it was 
 */
const normalizePath = path =>
    path.startsWith('./') ?
        path.slice(2) 
        :
        path
/**
 * Export of native functions to Elm side
 */
const _AIRTucha$board$Native_File = function() {
    return {
        /**
         * @param {string} path Address of file to read
         * @return Elm Task which inclose file which hidden inside function, to prevent issues on Elm side
         */
        read: path => scheduler.nativeBinding( callback => 
                fs.readFile( path, ( error, content ) =>
                    error ?
                        callback( scheduler.fail( error.toString() ) )
                    : 
                        callback( scheduler.succeed( func => func( content ) ) )
        )),
        /**
         * @param {string} path Address of file to write
         * @param {Elm File} file Elm representation of file
         * @return Task with inclose the same File
         */
        write: path => file => scheduler.nativeBinding( 
            callback =>
                file( buffer => 
                    fs.writeFile( normalizePath(path), buffer, error =>
                        error ?
                            callback( scheduler.fail( error ) )
                        :
                            callback( scheduler.succeed( file ) )
        ))),
        /**
         * @param {string} encoding File encoding by specially formated string
         * @param {JS Buffer} buffer Data JS byte buffer 
         * @returns String which represent the bytes in specified encoding
         */
        string: encoding => buffer => buffer.toString( encoding.ctor.toLocaleLowerCase() ),
        /**
         * @param {string} str String which has to be packed in a function to represent file
         * @returns Function which inclose file to hide it from Elm side
         */
        fromString: str => (func => func( new Buffer(str) ))
    }
}()