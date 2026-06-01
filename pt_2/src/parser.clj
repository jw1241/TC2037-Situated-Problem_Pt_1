(ns parser
  (:require
    [lexer :as lex]
    [clojure.string :as str]))

;Identify number
(defn number-node [value]
    {:type :number
        :value value})

;Identify identifier
(defn identifier-node [name]
    {:type :identifier
    :name name})

; Identify binary operations
(defn binary-node [op left right]
    {:type :binary-op
        :operator op
        :left left
        :right right})

;Identify assignments
(defn assignment-node [var expr]
    {:type :assignment
        :variable var
        :value expr})

;Identify print
(defn print-node [expr]
    {:type :print
        :expression expr})

;Identify string
(defn string-node [value]
    {:type :string
        :value value})

;Remove comment tokens
(defn remove-comments [tokens]
    (filter #(not= :COMMENT (:type %)) tokens))

;Parse list
(defn parse-list [tokens]

    {:type :list

    :elements
    (mapv
        #(Integer/parseInt (:value %))
        (filter
        #(= :NUMBER (:type %))
        tokens))
    }
)

;Parse expression
(defn parse-expression [tokens]

    (cond

    (= 1 (count tokens))

    (let [tok (first tokens)]

        (case (:type tok)

        :NUMBER
        (number-node
            (Integer/parseInt (:value tok))
        )

        :STRING
        (string-node
            (:value tok))

        :IDENTIFIER
        (identifier-node
            (:value tok))

        nil))

    (= 3 (count tokens))

    (binary-node
        (:value (second tokens))
        (parse-expression [(first tokens)]
            )
        (parse-expression [(nth tokens 2)]
            )
        )
    )
)

;Parse sqrt
(defn parse-sqrt [tokens]

    {:type :sqrt

        :argument
        (parse-expression
            [(nth tokens 4)]
        )
    }
)

;Parse variable assignment
(defn parse-assignment [tokens]

    {:type :assignment

        :variable
        (:value (first tokens))

        :value
        (parse-expression (drop 2 tokens))
    }
)

;Identify print
(defn parse-print [tokens]

    {:type :print

        :expression
        (parse-expression
            [(nth tokens 2)]
            )
        }
    )

;Parse the line
(defn parse-statement [tokens]
    (let [tokens (vec (remove-comments tokens))
        ]
    (cond

        (= "print"
            (:value (first tokens))
        )

        (parse-print tokens)

        (some #(= "=" (:value %))
            tokens)

        (parse-assignment tokens)

        :else

        {:type :unknown})
    )
)

;Parse file
(defn parse-program [filename]
    (mapv parse-statement
        (lex/tokenize-file filename))
    )