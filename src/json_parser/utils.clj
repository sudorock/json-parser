(ns json-parser.utils)
(refer 'clojure.string :only '[starts-with? split])

(def esc-char {\\ \\, \t \tab, \n \newline, \f \formfeed, \b \backspace, \r \return, \" \", \/ \/})
(defn get-esc [s] (let [esc (get esc-char (second s))] (if (some? esc) esc false)))
(defn trim-s [s] (try (clojure.string/trim s) (catch Exception e nil)))
(let [num-regex #"^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"]
  (defn get-num [s] (re-find num-regex s))
  (defn split-num [s] (try ((clojure.string/split s num-regex) 1) (catch Exception e nil))))
