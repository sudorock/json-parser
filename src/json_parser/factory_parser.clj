(ns json-parser.factory-parser
  (:require [json-parser.core :refer [bool-parser null-parser num-parser str-parser arr-parser obj-parser]]))

(defn factory-parser [s]
      (condp #(clojure.string/starts-with? %2 %1) s
        "null" null-parser
        "true" bool-parser
        "false" bool-parser
        "\"" str-parser
        "[" arr-parser
        "{" obj-parser
        nil))