(ns json-parser.core)

(declare gen-parser)

(defn throw-error []
      "Parse Error")

(defn check-digit? [ch]
      (if ch
        (and (<= (int ch) 57) (>= (int ch) 48))
        nil))

(defn null-parser [string]
      (if (clojure.string/starts-with? string "null")
        [nil (subs string 4)]
        nil))

(defn boolean-parser [string]
      (cond
        (clojure.string/starts-with? string "true") [true (subs string 4)]
        (clojure.string/starts-with? string "false") [false (subs string 5)]
        :else nil))

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
           (nil? fst) (let [res (+ prev (Double/parseDouble result))] (if (empty? res) [res nil] [res s]))
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
      (let [fst (first string) snd (second string)]
       (cond
         (and (= fst \0) ((complement check-digit?) snd)) (let [remain (subs string 1)] (if (empty? remain) [0 nil] [0 remain]))
         (or (clojure.string/starts-with? string "0.") (check-digit? fst)) (get-number string)
         (or (= fst \0) (= fst \.)) nil
         (= fst \+) (get-number (subs string 1))
         (= fst \-) (update (get-number (subs string 1)) 0 #(- %))
         :else nil)))

(defn string-parser [string]
      (if ((complement clojure.string/starts-with?) string "\"")
        nil
        (loop [[fst & rst] (rest string) result ""]
          (cond
            (= fst \") (let [remain (subs string (+ 2 (count result)))] (if (empty? remain) [result nil] [result remain]))
            ;(or (= fst \n) (= fst \t)) nil
            :else (recur rst (str result fst))))))

(defn get-array-vals [string]
      (let [s (clojure.string/trim string)]
          (if (= (first s) \,)
          (throw-error)
          (loop [remain s, result []]
            (let [fst (first remain) rst (clojure.string/trim (subs remain 1))]
              (cond
                (nil? fst) (throw-error)
                (= fst \]) (if (empty? rst) [result nil] [result rst])
                (= fst \,) (if-let [[val remaining] (gen-parser rst)]
                             (recur (clojure.string/trim remaining) (conj result val))
                             (throw-error))
                :else (if-let [[val remaining] (gen-parser remain)]
                        (recur (clojure.string/trim remaining) (conj result val))
                        (throw-error))))))))

(defn array-parser [string]
      (cond
        (= (first string) \[) (get-array-vals (subs string 1))
        :else nil))

(defn get-object-vals [string]
      (let [s (clojure.string/trim string)]
        (if (= (first s) \,)
          (throw-error)
          (loop [remain s, key nil, value nil, result {}]
            (let [fst (first remain) rst (clojure.string/trim (subs remain 1))]
              (cond
                (nil? fst) (throw-error)
                (= fst \}) (let [result (conj result (hash-map key value))] (if (empty? rst) [result nil] [result rst]))
                (= fst \:) (if-let [[val remaining] (gen-parser rst)]
                             (recur (clojure.string/trim remaining) key val result)
                             (throw-error))
                (= fst \,) (if-let [[val remaining] (string-parser rst)]
                             (recur (clojure.string/trim remaining) val nil (conj result (hash-map key value)))
                             (throw-error))
                :else (if-let [[val remaining] (string-parser remain)]
                        (recur (clojure.string/trim remaining) val value result)
                        (throw-error))))))))

(defn object-parser [string]
      (cond
        (= (first string) \{) (get-object-vals (subs string 1))
        :else nil))

(defn gen-parser [string]
      (let [null-parsed (null-parser string)
            boolean-parsed (boolean-parser string)
            string-parsed (string-parser string)
            number-parsed (number-parser string)
            array-parsed (array-parser string)
            object-parsed (object-parser string)]
        (or null-parsed boolean-parsed string-parsed number-parsed array-parsed object-parsed)))

(defn json-parser [s]
      (let [string (clojure.string/trim s) start (first string) end (get string (dec (count string)))]
        (cond
          (and (= start \[) (= end \])) (let [[result rst] (array-parser string)] (if rst (throw-error) result))
          (and (= start \{) (= end \})) (let [[result rst] (object-parser string)] (if rst (throw-error) result))
          :else (throw-error))))

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
