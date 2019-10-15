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

(defn string-parser [string]
      (if ((complement starts-with?) string "\"")
        nil
        (loop [rst (subs string 1) result ""]
          (let [fst (first rst)]
           (cond
             (or (= fst \tab) (= fst \newline)) nil
             (= fst \") (let [remain (subs rst 1)] (if (empty? remain) [result nil] [result remain]))
             :else (recur (subs rst 1) (str result fst)))))))

(defn get-arr-obj-vals [string arr]
      (let [[end? comma-colon? parser] (if arr
                                         [#(= % \]) #(= % \,) [gen-parser gen-parser]]
                                         [#(= % \}) #(or (= % \,) (= % \:)) [string-parser gen-parser]])]
        (if (= (first string) \,)
        (throw-error)
        (loop [[val remain] ((parser 0) (trim string)), result [], tgle 1]
          (if remain
            (let [trimmed (trim remain) fst (first trimmed) rst (trim (subs trimmed 1)) res (conj result val)]
              (cond
                 (end? fst) (if (empty? rst) [res nil] [res rst])
                 (comma-colon? fst) (recur ((parser (mod tgle 2)) rst) res (inc tgle))
                 :else (throw-error)))
            (throw-error))))))

(defn arr-obj-parser [s]
      (let [string (trim s) fst (first string)]
        (cond
          (= fst \[) (get-arr-obj-vals (subs string 1) true)
          (= fst \{) (update (get-arr-obj-vals (subs string 1) false) 0 #(apply hash-map %))
          :else nil)))

(defn gen-parser [s]
      (let [null (null-parser s) bool (boolean-parser s) string (string-parser s)
            number (number-parser s) arr-obj (arr-obj-parser s)]
        (or null bool string number arr-obj)))

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
        (> num 4) result
        :else (recur (inc num) (conj result (slurp (clojure.string/replace path "@" (str num)))))))))

(defn test-parser [test-cases]
      (loop [test-case 0]
        (if
          (>= test-case (count test-cases))
          nil
          (do
            (println test-case (test-cases test-case) (json-parser (test-cases test-case)))
            (recur (inc test-case))))))
