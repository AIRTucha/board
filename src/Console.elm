module Console exposing (println)

import Native.Console

println : a -> a
println = Native.Console.println