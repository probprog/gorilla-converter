# gorilla-converter

This is a converter for [Gorilla](http://gorilla-repl.org/) worksheets.
Currently it supports to convert from and to
[Clojupter](https://github.com/clojupyter/clojupyter), but might provide other
converters in the future. 

## Clojupyter support

The converter only copies user supplied input. This means you need to rerun the
notebook in jupyter after you have converted. This behaviour makes sense,
because notebook behaviour should be reproducible anyway and you should check
that it executes correctly after conversion.


## Usage

You can run the converter from the command line like this:

~~~bash
lein run -i gmm-iris-roundtrip.clj -o gmm-iris-roundtrip.ipynb convert
~~~

You can also use the plain functions to convert strings:

~~~clojure
(require '[gorilla-converter.gorilla :as gorilla]
         '[gorilla-converter.jupyter :as jupyter])
		 
;; ... 

(gorilla/gorilla->jupyter gorilla-worksheet-string)

;; ...

(jupyter/jupyter->gorilla jupyter-worksheet-json-string)
~~~

## Extension

It is possible to implement your own exporter for Gorilla, with the help of an
[instaparse](https://github.com/Engelberg/instaparse) transform.


## TODO

- provide a common environment for Gorilla and Clojupyter, so that worksheets
  can be migrated fully automatically

## License

Copyright © 2018 Christian Weilbach, Shriphani Palakodety

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
