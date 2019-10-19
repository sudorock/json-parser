(ns json-parser.core)

(refer 'clojure.string :only '[trim starts-with? split])
(declare gen-parser bool-parser null-parser number-parser string-parser array-parser object-parser)

;; utils ;;
(def parsers (vector bool-parser null-parser number-parser string-parser array-parser object-parser))
(def esc-char {\\ \\, \t \tab, \n \newline, \f \formfeed, \b \backspace, \r \return, \" \", \/ \/})
(defn get-esc [rst] (let [esc (get esc-char (second rst))] (if (some? esc) esc false)))
(defn check-digit? [ch] (when ch (and (<= (int ch) 57) (>= (int ch) 48))))
(defn throw-error [] "Parse Error")
(defn resultify [result remaining] (if (empty? remaining) [result nil] [result remaining]))
(defn json-cond? [parsed remaining] (and (not remaining) (or (vector? parsed) (map? parsed))))
(defn trim-first [s] (clojure.string/replace-first s #"^\s+" ""))
(defn get-num [s] (re-find #"^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?" s))
(defn split-num [s] (clojure.string/split s #"^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"))

(defn number-parser [s]
      (when-let [num-string (get-num s)]
        (try (resultify (Integer/parseInt num-string) ((split-num s) 1))
             (catch Exception e (resultify (Double/parseDouble num-string) ((split-num s) 1))))))

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

(defn null-parser [string] (when (starts-with? string "null") [nil (subs string 4)]))
(defn bool-parser [string] (condp #(starts-with? %2 %1) string "true" [true (subs string 4)] "false" [false (subs string 5)] nil))
(defn array-parser [string] (let [s (trim string)] (when (= (first s) \[) (get-array-vals (trim (subs s 1))))))

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
        "true" bool-parser
        "false" bool-parser
        "\"" string-parser
        "[" array-parser
        "{" object-parser
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


;; number parser in a regex
;; -?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?

