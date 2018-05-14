(ns gorilla-converter.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [gorilla-converter.gorilla :as gorilla]
            [gorilla-converter.jupyter :as jupyter])
  (:gen-class true))


(defn usage [options-summary]
  (->> ["This is a conversion program for Gorilla worksheets."
        ""
        "Usage: gorilla-converter [options]"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  convert  Convert a worksheet"
        ""
        "Please refer to the README for more information."]
       (str/join \newline)))


(def cli-options
  [["-i" "--input PATH" "Input file path (required)."
    :parse-fn #(io/file %)
    :validate [#(.exists %) "Input file must exist."]]
   ["-o" "--output PATH" "Output file path (required)."
    :parse-fn #(io/file %)
    :validate [#(not (.exists %)) "Output file already exists."]]
   ["-h" "--help"]])


(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (and (= 1 (count arguments))
           (#{"convert"} (first arguments))
           (:output options)
           (or (and (.endsWith (.getPath (:input options)) ".clj")
                    (.endsWith (.getPath (:output options)) ".ipynb"))
               (and (.endsWith (.getPath (:input options)) ".ipynb")
                    (.endsWith (.getPath (:output options)) ".clj"))))
      {:action (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))


(defn convert [options]
  (let [input (slurp (:input options))
        path (.getPath (:input options))]
    (spit (.getPath (:output options))
          (cond (.endsWith path ".clj")
                (gorilla/gorilla->jupyter input)
                :else
                (jupyter/jupyter->gorilla input)))))


(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "convert"  (convert options)))))


(comment
  #_(-main "-i" "/home/christian/Development/gorilla-converter/gmm-iris-roundtrip.clj"
         "-o" "/home/christian/Development/gorilla-converter/gmm-iris-roundtrip.ipynb"
         "convert"))







