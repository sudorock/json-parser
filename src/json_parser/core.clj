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

;>>>>>>> refactor
;<<<<<<< HEAD
;(nil? fst) (if (empty? s) [(Integer/parseInt result) nil] [(Integer/parseInt result) s])
;(check-digit? fst) (recur (subs s 1) (str result fst))
;(= fst \.) (number-after-point (Double/parseDouble result) (subs s 1))
;(or (= fst \e) (= fst \E)) (number-after-e (Double/parseDouble result) (subs s 1))
;:else [(Integer/parseInt result) s]))))
;
;(defn number-parser [string]
;      (let [fst (first string) snd (second string) third (get string 2)]
;        (cond
;          (= fst \0) (cond
;                       (check-digit? snd) nil
;                       (= snd \.) (if (check-digit? third) (get-number string) nil)
;                       :else (let [remain (subs string 1)] (if (empty? remain) [0 nil] [0 remain])))
;          (check-digit? fst) (get-number string)
;          (= fst \+) (get-number (subs string 1))
;          (= fst \-) (update (get-number (subs string 1)) 0 #(- %))
;          :else nil)))
;
;(defn string-parser [string]
;      (if ((complement starts-with?) string "\"")
;        nil
;        (loop [rst (subs string 1) result ""]
;          (let [fst (first rst)]
;            (cond
;              (or (= fst \tab) (= fst \newline)) nil
;              (= fst \") (let [remain (subs rst 1)] (if (empty? remain) [result nil] [result remain]))
;              :else (recur (subs rst 1) (str result fst)))))))
;
;(defn get-arr-obj-vals [string arr]
;      (let [[end? comma-colon? parser] (if arr
;                                         [#(= % \]) #(= % \,) [gen-parser gen-parser]]
;                                         [#(= % \}) #(or (= % \,) (= % \:)) [string-parser gen-parser]])]
;        (if (= (first string) \,)
;          (throw-error)
;          (loop [[val remain] ((parser 0) (trim string)), result [], tgle 1]
;            (if remain
;              (let [trimmed (trim remain) fst (first trimmed) rst (trim (subs trimmed 1)) res (conj result val)]
;                (cond
;                  (end? fst) (if (empty? rst) [res nil] [res rst])
;                  (comma-colon? fst) (recur ((parser (mod tgle 2)) rst) res (inc tgle))
;                  :else (throw-error)))
;              (throw-error))))))
;
;(defn arr-obj-parser [s]
;      (let [string (trim s) fst (first string)]
;        (cond
;          (= fst \[) (get-arr-obj-vals (subs string 1) true)
;          (= fst \{) (update (get-arr-obj-vals (subs string 1) false) 0 #(apply hash-map %))
;          :else nil)))
;
;(defn gen-parser [s]
;      (let [null (null-parser s) bool (boolean-parser s) string (string-parser s)
;            number (number-parser s) arr-obj (arr-obj-parser s)]
;        (or null bool string number arr-obj)))
;
;(defn json-parser [s]
;      (let [string (trim s) [parsed remaining] (gen-parser string)]
;        (if (or (vector? parsed) (map? parsed))
;          (if remaining (throw-error) parsed)
;          (throw-error))))
;
;;; Testing ;;
;
;(def fail-cases
;  (let [path "test/test_cases/fail@.json"]
;    (loop [num 1 result []]
;      (cond
;        (> num 33) result
;        :else (recur (inc num) (conj result (slurp (clojure.string/replace path "@" (str num)))))))))
;=======