(ns gorilla-converter.jupyter
  (:require [cheshire.core :as json]))

(defmulti emit-cell :cell_type)

(defmethod emit-cell "code"
  [cell]
  (concat
   [";; @@\n"] ;; start tag
   (if-not (empty? (:source cell))
     (:source cell)
     ["\n"])
   [";; @@\n"] ;; end tag
   ["\n"]
   ))

(defmethod emit-cell "markdown"
  [cell]
  (concat
   [";; **\n"] ;; start tag
   (if-not (empty? (:source cell))
     (map (fn [l] (str ";;; " l)) (:source cell))
     ["\n"])
   [";; **\n"] ;; end tag
   ["\n"]))

(defn jupyter->gorilla [input]
  (let [{:keys [cells]} (json/parse-string input keyword)]
    (apply str
           ";; gorilla-repl.fileformat = 1\n\n"
           (mapcat emit-cell cells))))


