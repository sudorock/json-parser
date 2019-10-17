(ns json-parser.core)
(refer 'clojure.string :only '[trim starts-with?])

(declare gen-parser boolean-parser null-parser number-parser string-parser array-parser object-parser)
(def parsers (vector boolean-parser null-parser number-parser string-parser array-parser object-parser))

;; util ;;
(def esc-char {\\ \\, \t \tab, \n \newline, \f \formfeed, \b \backspace, \r \return, \" \", \/ \/})
(defn get-esc [rst] (let [esc (get esc-char (second rst))] (if (some? esc) esc false)))
(defn check-digit? [ch] (when ch (and (<= (int ch) 57) (>= (int ch) 48))))
(defn throw-error [] "Parse Error")
(defn resultify [result remaining] (if (empty? remaining) [result nil] [result remaining]))
(defn resultify-num [result s dbl] (if dbl (resultify (Double/parseDouble result) s) (resultify (Long/parseLong result) s)))
(defn e-cond? [e snd thd] (and (not e) (or (check-digit? snd) (and (or (= snd \+) (= snd \-)) (check-digit? thd)))))
(defn point-cond? [point snd] (and (not point) (check-digit? snd)))
(defn json-cond? [parsed remaining] (and (not remaining) (or (vector? parsed) (map? parsed))))

;; parser ;;
(defn null-parser [string] (when (starts-with? string "null") [nil (subs string 4)]))

(defn boolean-parser [string] (condp #(starts-with? %2 %1) string "true" [true (subs string 4)] "false" [false (subs string 5)] nil))

(defn get-number [string]
      (loop [s (subs string 1), result (str (first string)), point false, dbl false, e false]
        (let [fst (first s), snd (second s), thd (second (rest s))]
          (cond
            (nil? fst) (resultify-num result s dbl)
            (check-digit? fst) (recur (subs s 1) (str result fst) point dbl e)
            (= fst \.) (if (point-cond? point snd)
                         (recur (subs s 1) (str result fst) true true e)
                         (resultify-num result s dbl))
            (or (= fst \e) (= fst \E)) (if (e-cond? e snd thd)
                                         (recur (subs s 2) (str result fst snd) point true true)
                                         (resultify-num result s dbl))
            :else (resultify-num result s dbl)))))

(defn number-parser [string]
      (let [fst (first string), snd (second string), third (get string 2)]
        (cond
          (= fst \0) (cond
                       (check-digit? snd) nil
                       (= snd \.) (when (check-digit? third) (get-number string))
                       :else (resultify 0 (subs string 1)))
          (or (= fst \+) (= fst \-) (check-digit? fst)) (get-number string)
          :else nil)))

(defn string-parser [string]
      (when (starts-with? string "\"")
        (loop [rst (subs string 1), result ""]
          (let [fst (first rst), esc (when (= fst \\) (get-esc rst))]
            (cond
              (and (= fst \\) (= (second rst) \u)) (recur (subs rst 6) (str result (read-string (subs rst 0 6))))
              (some? esc) (when esc (recur (subs rst 2) (str result esc)))
              (or (= fst \tab) (= fst \newline)) nil
              (= fst \") (resultify result (subs rst 1))
              :else (recur (subs rst 1) (str result fst)))))))


(defn get-array-vals [string]
      (condp = (first string) \, (throw-error) \] (resultify [] (trim (subs string 1)))
        (loop [[val remain] (gen-parser (trim string)), result []]
          (if remain
            (let [trimmed (trim remain) fst (first trimmed) rst (trim (subs trimmed 1)) res (conj result val)]
              (condp = fst
                \] (resultify res rst)
                \, (recur (gen-parser rst) res)
               (throw-error)))
            (throw-error)))))

(defn get-object-vals [string]
      (condp = (first string) \, (throw-error) \} (resultify {} (trim (subs string 1)))
        (loop [[val remain] (gen-parser (trim string)), result []]
          (if remain
            (let [trimmed (trim remain) fst (first trimmed) rst (trim (subs trimmed 1)) res (conj result val)]
              (condp = fst
                \} (resultify (apply hash-map res) rst)
                \: (recur (gen-parser rst) res)
                \, (recur (string-parser rst) res)
                (throw-error)))
            (throw-error)))))

(defn array-parser [string] (let [s (trim string)] (when (= (first s) \[) (get-array-vals (trim (subs s 1))))))

(defn object-parser [string] (let [s (trim string)] (when (= (first s) \{) (get-object-vals (trim (subs s 1))))))

(defn gen-parser [string] (some identity (map #(% string) parsers)))

(defn json-parser [string]
      (if-let [[parsed remaining] (gen-parser (trim string))]
        (if (json-cond? parsed remaining) parsed (throw-error))
        (throw-error)))

;; factory parser ;;

(defn factory-parser [s]
      (condp #(starts-with? %2 %1) s
        "null" null-parser
        "true" boolean-parser
        "false" boolean-parser
        "\"" string-parser
        "[" array-parser
        "{" object-parser
        (throw-error)))

;; Testing ;;

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
