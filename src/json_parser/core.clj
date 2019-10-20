(ns json-parser.core (:require [clojure.string :refer [trim starts-with? escape split]]))
(declare val-parser)

(defn trim-s [s] (try (trim s) (catch Exception e "")))
(defn throw-error [] "Parse Error")

(defn get-esc [s] (when-let [esc (re-find #"^\\(?:(?:[\\|t|n|f|b|r|\"|\/])|(?:u[a-fA-F\d]{4}))" s)]
                    [(escape (str (read-string esc)) {\t \tab \n \newline \f \formfeed \b \backspace \r \return}) (subs s (count esc))]))

(defn null-parser [s] (when (starts-with? s "null") [nil (subs s 4)]))
(defn bool-parser [s] (condp #(starts-with? %2 %1) s "true" [true (subs s 4)] "false" [false (subs s 5)] nil))

(defn num-parser [s]
      (when-let [num-str (re-find #"^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][-+]?\d+)?" s)]
        (try [(Integer/parseInt num-str) (subs s (count num-str))]
             (catch Exception e [(Double/parseDouble num-str) (subs s (count num-str))]))))

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

(defn json-parser [s] (if-let [[res rst] (val-parser s)]
                        (if (and (empty? rst) (or (vector? res) (map? res))) res (throw-error))
                        (throw-error)))