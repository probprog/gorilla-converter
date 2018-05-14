(ns gorilla-converter.core-test
  (:require [clojure.test :refer :all]
            [gorilla-converter.jupyter :as jupyter]
            [gorilla-converter.gorilla :as gorilla]))


(deftest roundtrip-test
  (testing "Test one roundtrip through the converter."
    (let [input (slurp "resources/gmm-iris-roundtrip.clj")]
      (is (= input (-> input
                      gorilla/gorilla->jupyter
                      jupyter/jupyter->gorilla
                      ))))))




