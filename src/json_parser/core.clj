(ns json-parser.core (:require [json-parser.utils :refer [get-num split-num trim-s get-esc]]))
(refer 'clojure.string :only '[starts-with?])
(declare value-parser)

;; utils ;;
(defn throw-error [] "Parse Error")
(defn resultify [result remaining] (if (empty? remaining) [result nil] [result remaining]))

(defn num-parser [s]
      (when-let [num-string (get-num s)]
        (try (resultify (Integer/parseInt num-string) (split-num s))
             (catch Exception e (resultify (Double/parseDouble num-string) (split-num s))))))

(defn str-parser [string]
      (when (starts-with? string "\"")
        (loop [rst (subs string 1), result ""]
          (let [fst (first rst), esc (when (= fst \\) (get-esc rst))]
            (cond
              (and (= fst \\) (= (second rst) \u)) (recur (subs rst 6) (str result (read-string (subs rst 0 6))))
              (some? esc) (when esc (recur (subs rst 2) (str result esc)))
              (or (= fst \tab) (= fst \newline)) nil
              (= fst \") (resultify result (subs rst 1))
              :else (recur (subs rst 1) (str result fst)))))))

(defn null-parser [string] (when (starts-with? string "null") [nil (subs string 4)]))
(defn bool-parser [string] (condp #(starts-with? %2 %1) string "true" [true (subs string 4)] "false" [false (subs string 5)] nil))

(defn arr-parser [s]
      (when (re-find #"^\[(?!\s*,)" s)
        (loop [rst (trim-s (subs s 1)), result []]
          ;(println result rst)
          (cond
            (nil? rst) (throw-error)
            (= (first rst) \]) (resultify result (subs rst 1))
            (= (first rst) \,) (let [[res remain] (value-parser (subs rst 1))] (recur (trim-s remain) (conj result res)))
            :else (let [[res remain] (value-parser rst)] (recur remain (conj result res)))))))

(defn obj-parser [s]
      (when (re-find #"^\{(?!\s*,)" s)
        (loop [rst (trim-s (subs s 1)), result []]
          (cond
            (nil? rst) (throw-error)
            (= (first rst) \}) (resultify (apply hash-map result) (subs rst 1))
            (= (first rst) \:) (let [[res remain] (value-parser (subs rst 1))] (recur (trim-s remain) (conj result res)))
            (= (first rst) \,) (let [[res remain] (str-parser (trim-s (subs rst 1)))] (recur (trim-s remain) (conj result res)))
            :else (let [[res remain] (str-parser rst)] (recur remain (conj result res)))))))

(defn value-parser [s] (some identity (map #(% (trim-s s)) [bool-parser null-parser num-parser str-parser arr-parser obj-parser])))

(defn json-parser [s]
      (if-let [[res rst] (value-parser s)]
        (if (and (not rst) (or (vector? res) (map? res))) res (throw-error))
        (throw-error)))

;; factory parser ;;

(defn factory-parser [s]
      (condp #(starts-with? %2 %1) s
        "null" null-parser
        "true" bool-parser
        "false" bool-parser
        "\"" str-parser
        "[" arr-parser
        "{" obj-parser
        (throw-error)))

;; testing ;;

(def fail-cases
  (let [path "test/test_cases/fail@.json"]
    (loop [num 1 result []]
      (if (> num 33)
        result
        (recur (inc num) (conj result (slurp (clojure.string/replace path "@" (str num)))))))))

(def pass-cases
  (let [path "test/test_cases/pass@.json"]
    (loop [num 1 result []]
      (if (> num 6)
        result
        (recur (inc num) (conj result (slurp (clojure.string/replace path "@" (str num)))))))))

(defn test-parser [test-cases]
      (loop [test-case 0]
        (when
          (< test-case (count test-cases))
          (do
            (println "Test case No.:" (inc test-case) \newline (test-cases test-case) \newline (json-parser (test-cases test-case)))
            (println "---------------------------------------")
            (recur (inc test-case))))))