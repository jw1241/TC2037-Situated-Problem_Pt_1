(ns main
  (:require [clojure.string :as str]
            [lexer :refer [tokenize-file]]
            [syntax-checker :refer [syntax-check]]
            [parser :refer [parse-program]]
            ))

(defn read-lines [filename]
  (str/split-lines (slurp filename)))

(defn -main []
    ;(println (read-lines "Sample_Code.py")))
    ;(println (tokenize-file "Sample_Code.py")))
    (println (parse-program "Sample_Code.py")))

  ;(doseq [err (syntax-check "Sample_Bad.py")]
  ;(if-let [line (:line err)]
    ;(println (str "Line " line ": " (:message err)))
    ;(println (:message err)))))


(-main)