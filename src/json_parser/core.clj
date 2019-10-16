(ns json-parser.core
  (:require [clojure.data.json :as json]))

(refer 'clojure.string :only '[trim starts-with?])

(declare gen-parser)

(def esc-char {\\ \\, \t \tab, \n \newline, \f \formfeed, \b \backspace, \r \return, \" \", \/ \/})

(defn check-digit? [ch] (if ch (and (<= (int ch) 57) (>= (int ch) 48)) nil))

(defn throw-error [] "Parse Error")

(defn resultify [result remaining] (if (empty? remaining) [result nil] [result remaining]))

(defn null-parser [s] (if (starts-with? s "null") [nil (subs s 4)] nil))

(defn boolean-parser [s] (condp #(starts-with? %2 %1) s "true" [true (subs s 4)] "false" [false (subs s 5)] nil))

(defn number-after-e [prev string]
      (let [after-e (first string), sign-exists (or (= after-e \-) (= after-e \+)), sign (case after-e \- - \+ + +)]
        (if (or (check-digit? after-e) sign-exists)
          (loop [s (if sign-exists (subs string 1) string) result ""]
            (let [fst (first s)]
              (cond
                (nil? fst) [(* prev (Math/pow 10 (sign (Long/parseLong result)))) nil]
                (check-digit? fst) (recur (subs s 1) (str result fst))
                :else [(* prev (Math/pow 10 (sign (Long/parseLong result)))) s])))
          (throw-error))))

(defn number-after-point [prev string]
      (loop [s string, result "0."]
        (let [fst (first s)]
         (cond
           (nil? fst) (resultify (+ prev (Double/parseDouble result)) s)
           (check-digit? fst) (recur (subs s 1) (str result fst))
           (or (= fst \e) (= fst \E)) (number-after-e (+ prev (Double/parseDouble result)) (subs s 1))
           :else [(+ prev (Double/parseDouble result)) s]))))

(defn get-number [string]
      (loop [s string result ""]
        (let [fst (first s)]
          (cond
            (nil? fst) (resultify (Long/parseLong result) s)
            (check-digit? fst) (recur (subs s 1) (str result fst))
            (= fst \.) (number-after-point (Double/parseDouble result) (subs s 1))
            (or (= fst \e) (= fst \E)) (number-after-e (Double/parseDouble result) (subs s 1))
            :else [(Long/parseLong result) s]))))

(defn number-parser [string]
      (let [fst (first string) snd (second string) third (get string 2)]
       (cond
         (= fst \0) (cond
                      (check-digit? snd) nil
                      (= snd \.) (if (check-digit? third) (get-number string) nil)
                      :else (resultify 0 (subs string 1)))
         (= fst \+) (get-number (subs string 1))
         (= fst \-) (update (get-number (subs string 1)) 0 #(- %))
         (check-digit? fst) (get-number string)
         :else nil)))

(defn get-esc [rst] (let [esc (get esc-char (second rst))] (if (some? esc) esc false)))

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

(defn gen-parser [s]
      (let [null (null-parser s) bool (boolean-parser s) string (string-parser s)
            number (number-parser s) array (array-parser s) obj (object-parser s)]
        (or null bool string number array obj)))

(defn json-parser [s]
      (let [string (trim s) [parsed remaining] (gen-parser string)]
        (if (or (vector? parsed) (map? parsed))
          (if remaining (throw-error) parsed)
          (throw-error))))

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
