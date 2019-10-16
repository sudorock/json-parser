(ns json-parser.core
  (:require [clojure.data.json :as json]))

(refer 'clojure.string :only '[trim starts-with?])

(declare gen-parser)

(defn throw-error [] "Parse Error")

(defn check-digit? [ch] (if ch (and (<= (int ch) 57) (>= (int ch) 48)) nil))

(defn null-parser [s] (if (starts-with? s "null") [nil (subs s 4)] nil))

(defn boolean-parser [s] (cond (starts-with? s "true") [true (subs s 4)] (starts-with? s "false") [false (subs s 5)] :else nil))

(defn number-after-e [prev string]
      (let [after-e (first string), sign-exists (or (= after-e \-) (= after-e \+)), sign (case after-e \- - \+ + +)]
        (if (or (check-digit? after-e) sign-exists)
          (loop [s (if sign-exists (subs string 1) string) result ""]
            (let [fst (first s)]
              (cond
                (nil? fst) [(* prev (Math/pow 10 (sign (Integer/parseInt result)))) nil]
                (check-digit? fst) (recur (subs s 1) (str result fst))
                :else [(* prev (Math/pow 10 (sign (Integer/parseInt result)))) s])))
          (throw-error))))

(defn number-after-point [prev string]
      (loop [s string, result "0."]
        (let [fst (first s)]
         (cond
           (nil? fst) (let [res (+ prev (Double/parseDouble result))] (if (empty? s) [res nil] [res s]))
           (check-digit? fst) (recur (subs s 1) (str result fst))
           (or (= fst \e) (= fst \E)) (number-after-e (+ prev (Double/parseDouble result)) (subs s 1))
           :else [(+ prev (Double/parseDouble result)) s]))))

(defn get-number [string]
      (loop [s string result ""]
        (let [fst (first s)]
          (cond
            (nil? fst) (if (empty? s) [(Integer/parseInt result) nil] [(Integer/parseInt result) s])
            (check-digit? fst) (recur (subs s 1) (str result fst))
            (= fst \.) (number-after-point (Double/parseDouble result) (subs s 1))
            (or (= fst \e) (= fst \E)) (number-after-e (Double/parseDouble result) (subs s 1))
            :else [(Integer/parseInt result) s]))))

(defn number-parser [string]
      (let [fst (first string) snd (second string) third (get string 2)]
       (cond
         (= fst \0) (cond
                      (check-digit? snd) nil
                      (= snd \.) (if (check-digit? third) (get-number string) nil)
                      :else (let [remain (subs string 1)] (if (empty? remain) [0 nil] [0 remain])))
         (check-digit? fst) (get-number string)
         (= fst \+) (get-number (subs string 1))
         (= fst \-) (update (get-number (subs string 1)) 0 #(- %))
         :else nil)))

(def esc-char {\\ \\, \t \tab, \n \newline, \f \formfeed, \b \backspace, \r \return, \" \", \/ \/, \u \u})

(defn get-esc [rst]
      (let [esc (get esc-char (second rst))]
        (if (some? esc) esc false)))

(defn get-hex [s]
      (read-string s))

(defn string-parser [string]
      (if ((complement starts-with?) string "\"")
        nil
        (loop [rst (subs string 1) result ""]
          (let [fst (first rst) esc (if (= fst \\) (get-esc rst) nil)]
           (cond
             (and (= fst \\) (= (second rst) \u)) (recur (subs rst 6) (str result (get-hex (subs rst 0 6))))
             (some? esc) (if (false? esc) nil (recur (subs rst 2) (str result esc)))
             (or (= fst \tab) (= fst \newline)) nil
             (= fst \") (let [remain (subs rst 1)] (if (empty? remain) [result nil] [result remain]))
             :else (recur (subs rst 1) (str result fst)))))))

(defn get-array-vals [string]
      (cond
        (= (first string) \,) (throw-error)
        (= (first string) \]) (let [rst (trim (subs string 1))] (if (empty? rst) [[] nil] [[] rst]))
        :else (loop [[val remain] (gen-parser (trim string)), result []]
                (if remain
                  (let [trimmed (trim remain) fst (first trimmed) rst (trim (subs trimmed 1)) res (conj result val)]
                    (cond
                     (= fst \]) (if (empty? rst) [res nil] [res rst])
                     (= fst \,) (recur (gen-parser rst) res)
                     :else (throw-error)))
                  (throw-error)))))

(defn get-object-vals [string]
      (cond
        (= (first string) \,) (throw-error)
        (= (first string) \}) (let [rst (trim (subs string 1))] (if (empty? rst) [{} nil] [{} rst]))
        :else (loop [[val remain] (gen-parser (trim string)), result []]
                (if remain
                  (let [trimmed (trim remain) fst (first trimmed) rst (trim (subs trimmed 1)) res (conj result val)]
                    (cond
                      (= fst \}) (let [result (apply hash-map res)] (if (empty? rst) [result nil] [result rst]))
                      (= fst \:) (recur (gen-parser rst) res)
                      (= fst \,) (recur (string-parser rst) res)
                      :else (throw-error)))
                  (throw-error)))))

(defn array-parser [s]
      (let [string (trim s)]
        (if (= (first string) \[)
          (get-array-vals (trim (subs string 1)))
          nil)))

(defn object-parser [s]
      (let [string (trim s)]
        (if (= (first string) \{)
          (get-object-vals (trim (subs string 1)))
          nil)))

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
