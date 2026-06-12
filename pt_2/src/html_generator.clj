(ns html-generator
  (:require [clojure.string :as str]
            [lexer :refer [tokenize-file]]))

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
      (format "<span class=\"%s\">%s</span>" clean-type safe-text))))

(defn line-to-html [line-tokens]
  (let [
        indent-count (:indent (first line-tokens) 0)
        indent-spaces (str/join "" (repeat indent-count " "))
        html-tokens (map token-to-html line-tokens)
        joined-line (str/join "" html-tokens)]
    (str indent-spaces joined-line "\n")))

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
                   "pre { font-family: monospace; font-size: 14px; line-height: 1.5; margin: 0; }\n"
                   "body { background-color: #272822; color: #f8f8f2; padding: 20px; }\n"
                   ".comment { color: #75715e; font-style: italic; }\n"
                   ".number { color: #ae81ff; }\n"
                   ".keyword { color: #f92672; font-weight: bold; }\n"
                   ".string { color: #e6db74; }\n"
                   ".identifier { color: #ffffff; }\n"
                   ".delimiter { color: #a6e22e; }\n"
                   ".operator { color: #66d9ef; }\n"
                   ".unknown { color: #ff0000; text-decoration: underline; }\n"
                   "</style>\n"
                   "</head>\n"
                   "<body>\n"
                   "<pre>" body-content "</pre>\n" 
                   "</body>\n"
                   "</html>\n")]
    (spit output-file full-page)))

(defn run [input-file output-file]
  (let [tokens (tokenize-file input-file)]
    (make-html tokens output-file)
    (println "All good")))
