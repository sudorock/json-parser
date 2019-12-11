(ns json-parser.core (:require [clojure.string :refer [trim starts-with? escape split]]))
(declare val-parser)

(defn get-esc [s]
  (when-let [esc (re-find #"^\\(?:(?:[\\|t|n|f|b|r|\"|\/])|(?:u[0-9a-fA-F]{4}))" s)]
    [(escape (str (read-string esc)) {\t \tab \n \newline \f \formfeed \b \backspace \r \return}) (subs s (count esc))]))

(defn null-parser [s] (when (starts-with? s "null") [nil (subs s 4)]))
(defn bool-parser [s] (when-let [bool (re-find #"^(?:true|false)" s)] [(read-string bool) (subs s (count bool))]))

(defn num-parser [s]
  (when-let [num-str (re-find #"^-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?" s)]
    (try [(Integer/parseInt num-str) (subs s (count num-str))]
         (catch Exception e [(Double/parseDouble num-str) (subs s (count num-str))]))))

(defn str-parser [s]
  (when (re-find #"^\s*\"" s)
    (loop [rst (subs (trim s) 1), result ""]
      (cond
        (= (first rst) \") [result (subs rst 1)]
        (= (first rst) \\) (when-let [[res rmn] (get-esc rst)] (recur rmn (str result res)))
        (or (= (first rst) \tab) (= (first rst) \newline)) nil
        :else (recur (subs rst 1) (str result (first rst)))))))

(defn arr-parser [s]
  (when (re-find #"^\[(?!\s*,)" s)
    (loop [rst (trim (subs s 1)), result []]
      (cond
        (empty? rst) nil
        (= (first rst) \]) [result (subs rst 1)]
        (= (first rst) \,) (when-let [[res rmn] (val-parser (subs rst 1))] (recur (trim rmn) (conj result res)))
        :else (when-let [[res rmn] (val-parser rst)] (recur rmn (conj result res)))))))

(defn obj-parser [s]
  (when (re-find #"^\{(?!\s*,)" s)
    (loop [rst (trim (subs s 1)), result []]
      (cond
        (empty? rst) nil
        (= (first rst) \}) [(apply hash-map result) (subs rst 1)]
        (= (first rst) \:) (when-let [[res rmn] (val-parser (subs rst 1))] (recur (trim rmn) (conj result res)))
        (= (first rst) \,) (when-let [[res rmn] (str-parser (subs rst 1))] (recur (trim rmn) (conj result res)))
        :else (when-let [[res rmn] (str-parser rst)] (recur rmn (conj result res)))))))

(defn val-parser [s]
  (reduce #(or %1 (%2 (trim s))) nil [bool-parser null-parser num-parser str-parser arr-parser obj-parser]))

(defn json-parser [s]
  (if-let [[res rst] (val-parser s)]
    (if (and (empty? rst) (or (vector? res) (map? res))) res "Parse Error")
    "Parse Error"))