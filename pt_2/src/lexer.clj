(ns lexer
  (:require [clojure.string :as str])
  )

;Read file
(defn read-lines [filename]
  (str/split-lines (slurp filename))
  )

;Token Structure
(defn token [type value line indent]
  {:type type
   :value value
   :line line
   :indent indent})

;Compute indentation
(defn indentation [line]
  (count (take-while #(= % \space) line))
  )

;Keywords in the limited implementation
(def keywords
  #{"if" "elif" "else"
    "for" "while"
    "in" "range"
    "print"
    "sqrt" "import" "math"})

;Operators
(def operators
  #{"+" "-" "*" "/" "="
    "==" "!="
    "<" ">"
    "<=" ">=" "+=" "-="
    "**"})

;Delimiters
(def delimiters #{"(" ")" "," ":" "[" "]" "."})

;Classify the type the character falls under
(defn classify [word]
  (cond
    (keywords word) :KEYWORD
    (operators word) :OPERATOR
    (delimiters word) :DELIMITER

    (re-matches #"\"[^\"]*\"" word)
      :STRING

    (re-matches #"-?\d+" word)
      :NUMBER

    (re-matches #"[A-Za-z_][A-Za-z0-9_]*" word)
      :IDENTIFIER

    :else
      :UNKNOWN))

;Split the characters up in each line
(defn split-line [line]
  (re-seq
   #"[A-Za-z_][A-Za-z0-9_]*|\"[^\"]*\"|-?\d+|==|!=|<=|>=|-=|\*\*|@|[\[\].:(),=+\-*/<>]"
   line))

;Recognize comments
(defn comment-part [line]
  (let [pos (.indexOf line "#")]
    (when (not= pos -1)
      (subs line pos))
    )
  )

;Extract only the code if the line has code and comment
(defn code-part [line]
  (let [pos (.indexOf line "#")]
    (if (= pos -1)
      line
      (subs line 0 pos))
    )
  )

;Tokenize one line
(defn tokenize-line [line line-num]
  (let [indent (indentation line)
        code (code-part line)
        comment (comment-part line)

        code-tokens
        (map #(token (classify %) % line-num indent)
             (split-line code))

        comment-token
        (when comment
          [(token :COMMENT comment line-num indent)]
          )
        ]

    (concat code-tokens comment-token)
    )
  )

;Tokenize entrie file
(defn tokenize-file [filename]
  (->> (read-lines filename)
    (map-indexed
      (fn [idx line]
        (tokenize-line line (inc idx))
        )
      )
    (remove empty?)
    )
  )