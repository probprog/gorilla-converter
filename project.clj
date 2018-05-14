(defproject gorilla-converter "0.1.0-SNAPSHOT"
  :description "A conversion utility from and to Gorilla worksheets"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.9"]
                 [cheshire "5.8.0"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main gorilla-converter.core)
