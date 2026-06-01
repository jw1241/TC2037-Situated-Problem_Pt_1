(ns syntax-checker
  (:require
    [lexer :as lex]
    [clojure.string :as str]))

(def keywords
  #{"if" "elif" "else"
    "for" "while"
    "in" "range"
    "print"
    "import"
    "math"})

;Get the value of the token
(defn first-value [tokens]
  (:value (first tokens))
  )

;Get the indentation line
(defn line-indent [tokens]
  (:indent (first tokens))
  )

;Get the line number
(defn line-num [tokens]
  (:line (first tokens))
  )

(def block-keywords
  #{"if" "elif" "else" "for" "while"})

;Identify unknown tokens
(defn check-unknown-tokens [tokens]
  (keep
    (fn [tok]
      (when (= (:type tok) :UNKNOWN)
        {:line (:line tok)
          :message
          (str "Unknown token '" (:value tok) "'")
          }
        )
      )
    tokens))

;Check if a colon is missing
(defn missing-colon? [tokens]
  (when (seq tokens)

    (let [first-token (first tokens)]

      (when (and
              (= (:type first-token) :KEYWORD)
              (block-keywords (:value first-token))
              (not= ":" (:value (last tokens))
              )
            )
        
        {:line (:line first-token)
          :message "Missing ':'"})
          )
        )
      )
        
;Are parentheses balanced
(defn check-parens [tokens]
  (let [opens
    (count
      (filter #(= "(" (:value %))
        tokens))

    closes
    (count
      (filter #(= ")" (:value %))
        tokens))
      ]

  (when (not= opens closes)
    {:line (:line (first tokens))
    :message "Unmatched parentheses"})
  )
)

;Check for proper indentation
(defn check-indentation [lines]
  (loop [remaining lines
    prev nil
      errors []]

    (if (empty? remaining)

      errors

      (let [current (first remaining)

        errors
        (if (and prev
          (= ":" (:value (last prev))
            )

          (<= (:indent (first current))
              (:indent (first prev))
            )
          )

          (conj errors
            {:line (:line (first current))
              :message
              "Expected indentation after block statement"})

          errors)]

          (recur
            (rest remaining)
            current
            errors))
          )
        )
      )

;Detect if a keyword is not spelled correctly
(defn check-unknown-keyword [tokens]

  (when (and (seq tokens)
    (= ":" (:value (last tokens))
    )
  )

  (let [first-token (first tokens)
        value (:value first-token)]

    (when (and (= :IDENTIFIER (:type first-token))
      (not (keywords value))
      )

      {:line (:line first-token)
        :message (str "Unknown keyword '" value "'")}
      )
    )
  )
)

;Error message
(defn syntax-error [line message]
  {:line line
   :message message})

;Is it a valid identifier
(defn valid-identifier? [s]
  (boolean
    (re-matches #"[A-Za-z_][A-Za-z0-9_]*" s))
  )

;Check if it's variable assignment
(defn assignment-line? [tokens]
  (some #(= (:value %) "=") tokens))

;Store value of the assignment
(defn check-assignment [tokens]
  (let [eq-pos
        (.indexOf
          (vec (map :value tokens))
          "=")]

    (when (>= eq-pos 0)

      (let [before
        (if (zero? eq-pos)
          nil
          (nth tokens (dec eq-pos))
          )
        ]

        (when (or (nil? before)
                  (not= (:type before)
                        :IDENTIFIER))

          {:line (:line (first tokens))
            :message
            "Expected identifier before '='"})
          )
        )
      )
    )

;Check if blocks are written correctly
(defn check-blocks [lines]

  (loop [remaining lines
         seen-if false
         errors []]

    (if (empty? remaining)

      errors

      (let [tokens (first remaining)
            keyword (first-value tokens)]

        (cond

          (= keyword "if")
          (recur
            (rest remaining)
            true
            errors)

          (= keyword "elif")
          (if seen-if

            (recur
              (rest remaining)
              true
              errors)

            (recur
              (rest remaining)
              false
              (conj errors
                {:line (line-num tokens)
                  :message "elif without matching if"})
                )
              )

          (= keyword "else")
          (if seen-if

            (recur
              (rest remaining)
              false
              errors)

            (recur
              (rest remaining)
              false
              (conj errors
                {:line (line-num tokens)
                  :message "else without matching if"})
                )
              )

          :else
          (recur
            (rest remaining)
            seen-if
            errors))
          )
        )
      )
    )

;Check whole file
(defn syntax-check [filename]

  (let [lines (lex/tokenize-file filename)

        errors
        (concat
          (keep missing-colon? lines)
          (keep check-parens lines)
          (mapcat check-unknown-tokens lines)
          (keep check-unknown-keyword lines)
          (keep check-assignment lines)
          (check-indentation lines)
          (check-blocks lines))

        sorted-errors
        (sort-by :line errors)]

    (if (empty? sorted-errors)

      [{:message "No syntax errors found"}]

      sorted-errors))
    )