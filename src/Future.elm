module Future exposing (..)

import Native.Future

type Future t e = 
    Future t e

apply : ( t -> s ) -> Future t e -> Future s e 
apply = Native.Future.bind

bind : ( t -> Future s ) -> Future t e -> Future s e 
bind =  Native.Future.bind