(ns json-parser.test (:require [json-parser.core :refer [json-parser]]))

(def fail-cases
  (let [path "test/test_cases/fail@.json"]
    (loop [num 1 result []]
      (if (> num 33)
        result
        (recur (inc num) (conj result (slurp (clojure.string/replace path "@" (str num)))))))))

(def pass-cases
  (let [path "test/test_cases/pass@.json"]
    (loop [num 1 result []]
      (if (> num 5)
        result
        (recur (inc num) (conj result (slurp (clojure.string/replace path "@" (str num)))))))))

(defn test-parser [test-cases]
  (loop [test-case 0]
    (when (< test-case (count test-cases))
      (do
        (println "Test case No.:" (inc test-case))
        (println (test-cases test-case))
        (println (json-parser (test-cases test-case)))
        (println "---------------------------------------")
        (recur (inc test-case))))))

