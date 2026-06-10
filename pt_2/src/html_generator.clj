(ns html-generator
  (:require [clojure.string :as str]
            [lexer :refer [get-tokens]]))

(defn escape-html [text-string]
  (-> text-string
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn token-to-html [token]
  (let [token-type (name (:type token))
        clean-type (str/lower-case token-type)
        safe-text  (escape-html (:value token))]
    (if (= (:type token) :WHITESPACE)
      (:value token)
      (format "<text class=\"%s\">%s</text>" clean-type safe-text))))

(defn line-to-html [line-tokens]
  (let [html-tokens (map token-to-html line-tokens)
        joined-line (str/join "" html-tokens)]
    (str joined-line "<br>\n")))

(defn all-lines-to-html [nested-tokens]
  (let [html-lines (map line-to-html nested-tokens)]
    (str/join "" html-lines)))

(defn make-html [program-tokens output-file]
  (let [body-content (all-lines-to-html program-tokens)
        full-page (str
                   "<!DOCTYPE html>\n"
                   "<html>\n"
                   "<head>\n"
                   "<meta charset=\"utf-8\">\n"
                   "<title>Syntax Highlighted Output</title>\n"
                   "<style>\n"
                   "text { font-family: 'Monokai', monospace; font-weight: bold;}\n"
                   "body { background-color: #272822; padding: 20px; line-height: 1.5; }\n"
                   ".comment { color: gray; font-style: italic; }\n"
                   ".number { color: #8f00ff; }\n"
                   ".keyword { color: #f92672; }\n"
                   ".condition { color: #ff9800; }\n"
                   ".loop { color: #ae81ff; }\n"
                   ".string { color: #e6db74; }\n"
                   ".identifier { color: #ffffff; }\n"
                   ".delimiter { color: #a6e22e; }\n"
                   ".assignop { color: #f8f8f2; }\n"
                   ".operation { color: #66d9ef; }\n"
                   ".logicalop { color: #fd971f; }\n"
                   ".comparisonop { color: #66d9ef; }\n"
                   ".builtin { color: burlywood; }\n"
                   ".unknown { color: #ff0000; text-decoration: underline; }\n"
                   "</style>\n"
                   "</head>\n"
                   "<body>\n"
                   body-content
                   "</body>\n"
                   "</html>\n")] 
    (spit output-file full-page)))

(defn run [input-file output-file] 
  (let [tokens (get-tokens input-file)]
    (make-html tokens output-file)
    (println "All good")))
