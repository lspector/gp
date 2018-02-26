# gp

This code defines and runs a genetic programming system on the problem of finding a function that fits a particular set of [x y] pairs.

The "evolvefn" files use floating-point data from x^2 + x + 1 and try to find an arithmetic formula. These files also contain the most detailed comments.

The "evolveweather" files use empirical weather data and also try to find an arithmetic formula.

The "even3parity" files use boolean data for the even-3-parity function and try to find a boolean formula.

The files ending in "zip" use a Clojure zipper data structure to manipulate program trees; the files without "zip" do not.

The aim here is mostly to demonstrate how genetic programming can be implemented in Clojure simply and clearly, and several things are done in somewhat inefficient and/or non-standard ways. But this should provide a reasonable starting point for developing more efficient/standard/capable systems. 

## Usage

If you have [leiningen](http://leiningen.org) installed, then you should be able to run `lein run` on your command line with a namespace following `run`, to run the code in that namespace. For example, to run the code in the `gp.even3parity_lexicase` namespace, execute `lein run gp.even3parity_lexicase`.

You should also be able to execute `lein gorilla` to use [Gorilla REPL](http://gorilla-repl.org) to edit and run the code in in your browser. Note, however, that the evolutionary loops defined here may not terminate, and Gorilla REPL doesn't handle non-termination very well -- you just have to close the window and quit the server with control-C.

See evolvefn.clj for descriptions of how the code works.

## License

Copyright (C) 2012-2018 Lee Spector

Distributed under the Eclipse Public License, the same as Clojure.
