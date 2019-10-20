(ns json-parser.core (:require [clojure.string :refer [trim starts-with? escape split]]))
(declare val-parser)

(defn trim-s [s] (try (trim s) (catch Exception e "")))
(defn throw-error [] "Parse Error")

(defn get-esc [s] (when-let [esc (re-find #"^\\(?:(?:[\\|t|n|f|b|r|\"|\/])|(?:u[a-fA-F\d]{4}))" s)]
                    [(escape (str (read-string esc)) {\t \tab \n \newline \f \formfeed \b \backspace \r \return}) (subs s (count esc))]))

(let [num-regex #"^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][-+]?\d+)?"]
  (defn get-num [s] (re-find num-regex s))
  (defn split-num [s] (try ((split s num-regex) 1) (catch Exception e nil))))

(defn null-parser [s] (when (starts-with? s "null") [nil (subs s 4)]))
(defn bool-parser [s] (condp #(starts-with? %2 %1) s "true" [true (subs s 4)] "false" [false (subs s 5)] nil))

(defn num-parser [s]
      (when-let [num-string (get-num s)]
        (try [(Integer/parseInt num-string) (split-num s)]
             (catch Exception e [(Double/parseDouble num-string) (split-num s)]))))

(defn str-parser [s]
      (when (starts-with? s "\"")
        (loop [rst (subs s 1), result ""]
         (cond
             (= (first rst) \\) (when-let [[res rmn] (get-esc rst)] (recur rmn (str result res)))
             (or (= (first rst) \tab) (= (first rst) \newline)) nil
             (= (first rst) \") [result (subs rst 1)]
             :else (recur (subs rst 1) (str result (first rst)))))))

(defn arr-parser [s]
      (when (re-find #"^\[(?!\s*,)" s)
        (loop [rst (trim-s (subs s 1)), result []]
          (cond
            (empty? rst) nil
            (= (first rst) \]) [result (subs rst 1)]
            (= (first rst) \,) (let [[res rmn] (val-parser (subs rst 1))] (recur (trim-s rmn) (conj result res)))
            :else (let [[res rmn] (val-parser rst)] (recur rmn (conj result res)))))))

(defn obj-parser [s]
      (when (re-find #"^\{(?!\s*,)" s)
        (loop [rst (trim-s (subs s 1)), result []]
          (cond
            (empty? rst) nil
            (= (first rst) \}) [(apply hash-map result) (subs rst 1)]
            (= (first rst) \:) (let [[res rmn] (val-parser (subs rst 1))] (recur (trim-s rmn) (conj result res)))
            (= (first rst) \,) (let [[res rmn] (str-parser (trim-s (subs rst 1)))] (recur (trim-s rmn) (conj result res)))
            :else (let [[res rmn] (str-parser rst)] (recur rmn (conj result res)))))))

(defn val-parser [s] (reduce #(or %1 (%2 (trim-s s))) nil [bool-parser null-parser num-parser str-parser arr-parser obj-parser]))

(defn json-parser [s]
      (if-let [[res rst] (val-parser s)]
        (if (and (empty? rst) (or (vector? res) (map? res))) res (throw-error))
        (throw-error)))

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


;#"^\\(?:(?:[\\|\t|\n|\f|\r|\"|\/])|(?:u[a-fA-F\d]{4}))"

;; /^\s*(-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?)\s*/

;; ^(-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?)

;; "^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"

;; "^\\u[ABCDEFabcdef\d]{4}"

;#"^(?:\\u[ABCDEFabcdef\d]{4})"
;#"^(?:[\\\\|\\t|\\n|\\f|\\r|\\b|\\\"|\\/])|(?:u[a-fA-F\d]{4})"

;(def esc-char {\\ \\, \t \tab, \n \newline, \f \formfeed, \b \backspace, \r \return, \" \", \/ \/})
;(defn get-esc [s] (let [esc (get esc-char (second s))] (if (some? esc) esc false)))

;(defn str-parser [s]
;      (when (clojure.string/starts-with? s "\"")
;        (loop [rst (subs s 1), result ""]
;          (let [fst (first rst), esc (when (= fst \\) (get-esc rst))]
;            (cond
;              (and (= fst \\) (= (second rst) \u)) (recur (subs rst 6) (str result (read-string (subs rst 0 6))))
;              (some? esc) (when esc (recur (subs rst 2) (str result esc)))
;              (or (= fst \tab) (= fst \newline)) nil
;              (= fst \") [result (subs rst 1)]
;              :else (recur (subs rst 1) (str result fst)))))))


;; #"^\\(?:(?:[\\|t|n|f|b|r|\"|\/])|(?:u[a-fA-F\d]{4}))"