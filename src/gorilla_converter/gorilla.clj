(ns gorilla-converter.gorilla
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [cheshire.core :as json]))

(defn uncomment
  [xs]
  (str/join
   #"\n"
   (map
    (fn [x]
      (subs x 4))
    (str/split-lines xs))))

(defn remove-open-close-tags
  [segment open-tag close-tag]
  (filter
   #(and
     (not= (first %) open-tag)
     (not= (first %) close-tag))
   segment))



(def gorilla-parser
  (insta/parser
   "worksheet = worksheetHeader segmentWithBlankLine*

    lineEnd = '\\n' / '\\r\\n'

    worksheetHeader = ';; gorilla-repl.fileformat = 1' lineEnd lineEnd

    segmentWithBlankLine = segment lineEnd?

    segment = freeSegment / codeSegment

    freeSegment = freeSegmentOpenTag stringNoDelim? freeSegmentCloseTag

    freeSegmentOpenTag = ';; **' lineEnd

    freeSegmentCloseTag = lineEnd ';; **' lineEnd

    codeSegment = codeSegmentOpenTag stringNoDelim? codeSegmentCloseTag consoleSection? outputSection?

    codeSegmentOpenTag = ';; @@' lineEnd

    codeSegmentCloseTag = lineEnd ';; @@' lineEnd

    outputSection = outputOpenTag stringNoDelim outputCloseTag

    outputOpenTag = ';; =>' lineEnd

    outputCloseTag = lineEnd ';; <=' lineEnd

    consoleSection = consoleOpenTag stringNoDelim consoleCloseTag

    consoleOpenTag = ';; ->' lineEnd

    consoleCloseTag = lineEnd ';; <-' lineEnd

    stringNoDelim = noDelimSubStr+

    delimiter = freeSegmentOpenTag / freeSegmentCloseTag / codeSegmentOpenTag / codeSegmentCloseTag / outputOpenTag / outputCloseTag / consoleOpenTag / consoleCloseTag

    noDelimSubStr = noDelimChar noDelimStr

    noDelimStr = #'.*'

    noDelimChar = !delimiter #'.|\\s'"))


(defn split-lines [segment]
  (if segment
    (->> (str/replace segment "\\n" "\n")
         str/split-lines
         (mapv #(str % "\n")))
    []))

(defn emit-free-segment
  [segment]
  {
   "cell_type" "markdown",
   "metadata" {},
   "source" (split-lines segment)
   })


(defn emit-console-segment
  [segment]
  {
   "cell_type" "markdown",
   "metadata" {},
   "source" [(str segment)]})

(defn emit-console-segment
  [output]
  ;; TODO stream output
  {
   "cell_type" "stream",
   "name" "stdout"
   "text" [(str output)]})

(defn emit-clojure-code
  [code-segment]
  {"cell_type" "code",
   "execution_count" 0,
   "metadata" {},
   "outputs" [{"output_type" "stream",
               "name" "stdout"
               "text" ["Please rerun with clojupyter to see this output."]}],
   "source" (split-lines code-segment)})

(defn emit-worksheet [segments]
  {"cells" (vec segments)
   "metadata" {
               "kernelspec" {
                             "display_name" "Clojure",
                             "language" "clojure",
                             "name" "clojure"
                             },
               "language_info" {
                                "file_extension" ".clj",
                                "mimetype" "text/x-clojure",
                                "name" "clojure",
                                "version" "1.8.0"
                                }
               },
   "nbformat" 4,
   "nbformat_minor" 2})

(defn emit-output [segments]
  ;; not supported
  nil)


(defn gorilla->jupyter
  [worksheet]
  (->> (gorilla-parser worksheet)
       (insta/transform
        {:worksheet (fn [& xs] (emit-worksheet (rest xs)))

         :segmentWithBlankLine (fn [& xs] (first xs))

         :segment (fn [& xs]
                    (first xs))

         :freeSegment (fn [& xs]
                        (emit-free-segment
                         (uncomment
                          (first
                           (remove-open-close-tags xs
                                                   :freeSegmentOpenTag
                                                   :freeSegmentCloseTag)))))

         :codeSegment (fn [& xs]
                        (let [code-segment
                              (remove-open-close-tags xs
                                                      :codeSegmentOpenTag
                                                      :codeSegmentCloseTag)]
                          (emit-clojure-code
                           (first code-segment))))

         :consoleSection (fn [& xs]
                           (emit-console-segment xs))

         :outputSection (fn [& xs]
                          (emit-output xs))

         :stringNoDelim (fn [& xs]
                          (apply str (mapcat (fn [[_ & r]]
                                               (map second r)) xs)))})
       json/generate-string))
