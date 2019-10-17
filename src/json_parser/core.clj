(ns json-parser.core)
(refer 'clojure.string :only '[trim starts-with?])

(declare gen-parser boolean-parser null-parser number-parser string-parser array-parser object-parser)
(def parsers (vector boolean-parser null-parser number-parser string-parser array-parser object-parser))

;; util functions ;;
(def esc-char {\\ \\, \t \tab, \n \newline, \f \formfeed, \b \backspace, \r \return, \" \", \/ \/})
(defn get-esc [rst] (let [esc (get esc-char (second rst))] (if (some? esc) esc false)))
(defn check-digit? [ch] (if ch (and (<= (int ch) 57) (>= (int ch) 48)) nil))
(defn throw-error [] "Parse Error")
(defn resultify [result remaining] (if (empty? remaining) [result nil] [result remaining]))

;; parsers ;;
(defn gen-parser [s] (some identity (map #(% s) parsers)))

(defn null-parser [s] (if (starts-with? s "null") [nil (subs s 4)] nil))

(defn boolean-parser [s] (condp #(starts-with? %2 %1) s "true" [true (subs s 4)] "false" [false (subs s 5)] nil))

;(defn number-after-e [prev string]
;      (let [after-e (first string), sign-exists (or (= after-e \-) (= after-e \+)), sign (case after-e \- - \+ + +)]
;        (if (or (check-digit? after-e) sign-exists)
;          (loop [s (if sign-exists (subs string 1) string) result ""]
;            (let [fst (first s)]
;              (cond
;                (nil? fst) [(* prev (Math/pow 10 (sign (Long/parseLong result)))) nil]
;                (check-digit? fst) (recur (subs s 1) (str result fst))
;                :else [(* prev (Math/pow 10 (sign (Long/parseLong result)))) s])))
;          (throw-error))))
;
;(defn number-after-point [prev string]
;      (loop [s string, result "0."]
;        (let [fst (first s)]
;         (cond
;           (nil? fst) (resultify (+ prev (Double/parseDouble result)) s)
;           (check-digit? fst) (recur (subs s 1) (str result fst))
;           (or (= fst \e) (= fst \E)) (number-after-e (+ prev (Double/parseDouble result)) (subs s 1))
;           :else [(+ prev (Double/parseDouble result)) s]))))

;(defn get-number [string]
;      (loop [s string result "" dbl false]
;        (let [fst (first s)]
;          (cond
;            (nil? fst) (if dbl (resultify (Double/parseDouble result) s) (resultify (Long/parseLong result) s))
;            (or (= fst \+) (check-digit? fst)) (recur (subs s 1) (str result fst))
;            (= fst \.) (number-after-point (Double/parseDouble result) (subs s 1))
;            (or (= fst \e) (= fst \E)) (number-after-e (Double/parseDouble result) (subs s 1))
;            :else [(Long/parseLong result) s]))))
;
;(defn number-parser [string]
;      (let [fst (first string) snd (second string) third (get string 2)]
;        (cond
;          (= fst \0) (cond
;                       (check-digit? snd) nil
;                       (= snd \.) (if (check-digit? third) (get-number string) nil)
;                       :else (resultify 0 (subs string 1)))
;          (= fst \+) (get-number (subs string 1))
;          (= fst \-) (update (get-number (subs string 1)) 0 #(- %))
;          (check-digit? fst) (get-number string)
;          :else nil)))

(defn resultify-num [result s dbl] (if dbl (resultify (Double/parseDouble result) s) (resultify (Long/parseLong result) s)))

(defn e-cond? [e snd thd] (and (not e) (or (check-digit? snd) (and (or (= snd \+) (= snd \-)) (check-digit? thd)))))
(defn point-cond? [point snd] (and (not point) (check-digit? snd)))

(defn get-number [string]
      (loop [s (subs string 1), result (str (first string)), point false, dbl false, e false]
        (let [fst (first s) snd (second s) thd (second (rest s))]
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
      (let [fst (first string) snd (second string) third (get string 2)]
        (cond
          (= fst \0) (cond
                       (check-digit? snd) nil
                       (= snd \.) (if (check-digit? third) (get-number string) nil)
                       :else (resultify 0 (subs string 1)))
          (or (= fst \+) (= fst \-) (check-digit? fst)) (get-number string)
          :else nil)))

(defn string-parser [string]
      (if ((complement starts-with?) string "\"")
        nil
        (loop [rst (subs string 1) result ""]
          (let [fst (first rst) esc (if (= fst \\) (get-esc rst) nil)]
           (cond
             (and (= fst \\) (= (second rst) \u)) (recur (subs rst 6) (str result (read-string (subs rst 0 6))))
             (some? esc) (if (false? esc) nil (recur (subs rst 2) (str result esc)))
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

(defn array-parser [s] (let [string (trim s)] (if (= (first string) \[) (get-array-vals (trim (subs string 1))) nil)))

(defn object-parser [s] (let [string (trim s)] (if (= (first string) \{) (get-object-vals (trim (subs string 1))) nil)))

(defn json-parser [s]
      (let [string (trim s) [parsed remaining] (gen-parser string)]
        (if (or (vector? parsed) (map? parsed))
          (if remaining (throw-error) parsed)
          (throw-error))))

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
      (cond
        (> num 33) result
        :else (recur (inc num) (conj result (slurp (clojure.string/replace path "@" (str num)))))))))

(def pass-cases
  (let [path "test/test_cases/pass@.json"]
    (loop [num 1 result []]
      (cond
        (> num 6) result
        :else (recur (inc num) (conj result (slurp (clojure.string/replace path "@" (str num)))))))))

(defn test-parser [test-cases]
      (loop [test-case 0]
        (if
          (>= test-case (count test-cases))
          nil
          (do
            (println "Test case No.:" (inc test-case) \newline (test-cases test-case) \newline (json-parser (test-cases test-case)))
            (println "---------------------------------------")
            (recur (inc test-case))))))
