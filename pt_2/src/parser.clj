(ns parser
  (:require
   [lexer :as lex]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST NODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn number-node [value]
  {:type :number
   :value value})

(defn string-node [value]
  {:type :string
   :value value})

(defn identifier-node [name]
  {:type :identifier
   :name name})

(defn binary-node [op left right]
  {:type :binary-op
   :operator op
   :left left
   :right right})

(defn compare-node [op left right]
  {:type :comparison
   :operator op
   :left left
   :right right})

(defn list-node [elements]
  {:type :list
   :elements elements})

(defn assignment-node [var expr]
  {:type :assignment
   :variable var
   :value expr})

(defn print-node [expr]
  {:type :print
   :expression expr})

(defn call-node [name args]
  {:type :call
   :name name
   :args args})

(defn for-node [var limit body]
  {:type :for
   :variable var
   :limit limit
   :body body})

(defn while-node [condition body]
  {:type :while
   :condition condition
   :body body})

(defn if-node [condition then-body elifs else-body]
  {:type :if
   :condition condition
   :then then-body
   :elifs elifs
   :else else-body})

(defn minus-assignment-node [var expr]
  {:type :minus-assignment
   :variable var
   :value expr})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn indent-of [line]
  (:indent (first line)))

(defn collect-block
  [lines start-index parent-indent]

  (loop [i start-index
         body []]

    (println "COLLECT-BLOCK i =" i)

    (if (or (>= i (count lines))
            (<= (indent-of (nth lines i)) parent-indent))

      (do
        (println "RETURNING" i)
        [body i])

      (recur
       (inc i)
       (conj body (nth lines i))))))

(defn remove-comments [tokens]
  (vec
   (filter #(not= :COMMENT (:type %))
           tokens)))

(declare parse-expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-list [tokens]

  (list-node
   (mapv
    #(number-node
      (Integer/parseInt (:value %)))
    (filter #(= :NUMBER (:type %))
            tokens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-primary [tokens]

  (cond

    ;; math.sqrt(4)

    (and
     (>= (count tokens) 6)
     (= "math" (:value (first tokens)))
     (= "." (:value (second tokens)))
     (= "sqrt" (:value (nth tokens 2))))

    [(call-node
      "math.sqrt"
      [(number-node
        (Integer/parseInt
         (:value (nth tokens 4))))])
     []]

    ;; list

    (= "[" (:value (first tokens)))

    [(parse-list tokens) []]

    :else

    (let [tok (first tokens)]

      (case (:type tok)

        :NUMBER
        [(number-node
          (Integer/parseInt (:value tok)))
         (rest tokens)]

        :STRING
        [(string-node (:value tok))
         (rest tokens)]

        :IDENTIFIER
        [(identifier-node (:value tok))
         (rest tokens)]

        [nil nil]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARENTHESES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-paren [tokens]

  (if (= "(" (:value (first tokens)))

    (let [[expr remaining]
          (parse-expression (rest tokens))]

      [expr
       (rest remaining)])

    (parse-primary tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POWER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-power [tokens]

  (let [[left remaining]
        (parse-paren tokens)]

    (if (and (seq remaining)
             (= "**"
                (:value (first remaining))))

      (let [[right rest2]
            (parse-power (rest remaining))]

        [(binary-node "**" left right)
         rest2])

      [left remaining])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * /
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-term [tokens]

  (loop [[left remaining]
         (parse-power tokens)]

    (if (and (seq remaining)
             (#{"*" "/"}
              (:value (first remaining))))

      (let [op (:value (first remaining))
            [right rest2]
            (parse-power (rest remaining))]

        (recur
         [(binary-node op left right)
          rest2]))

      [left remaining])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; + -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-additive [tokens]

  (loop [[left remaining]
         (parse-term tokens)]

    (if (and (seq remaining)
             (#{"+" "-"}
              (:value (first remaining))))

      (let [op (:value (first remaining))
            [right rest2]
            (parse-term (rest remaining))]

        (recur
         [(binary-node op left right)
          rest2]))

      [left remaining])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPARISONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-comparison [tokens]

  (let [[left remaining]
        (parse-additive tokens)]

    (if (and (seq remaining)
             (#{"==" ">" "<"}
              (:value (first remaining))))

      (let [op (:value (first remaining))
            [right rest2]
            (parse-additive (rest remaining))]

        [(compare-node op left right)
         rest2])

      [left remaining])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENTRY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-expression [tokens]
  (parse-comparison tokens))

(defn parse-minus-assignment [tokens]

  (let [[expr _]
        (parse-expression
         (drop 2 tokens))]

    (minus-assignment-node
     (:value (first tokens))
     expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASSIGNMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-assignment [tokens]

  (let [[expr _]
        (parse-expression
         (drop 2 tokens))]

    (assignment-node
     (:value (first tokens))
     expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-print [tokens]
  (let [[expr _]
        (parse-expression
         (subvec (vec tokens) 2 (dec (count tokens))))]

    (print-node expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-statement [tokens]
  (let [tokens (remove-comments tokens)]

    (cond
      (empty? tokens)
      nil

      (= "print" (:value (first tokens)))
      (parse-print tokens)

      (= "-=" (:value (second tokens)))
      (parse-minus-assignment tokens)

      (some #(= "=" (:value %)) tokens)
      (parse-assignment tokens)

      :else
      {:type :unknown
       :raw tokens})))

(defn parse-for-header [tokens]

  {:variable (:value (second tokens))

   :limit
   (Integer/parseInt
    (:value (nth tokens 5)))})

(defn parse-while-header [tokens]

  (let [[condition _]
        (parse-comparison
         (butlast (rest tokens)))]

    condition))

(defn parse-if-header [tokens]

  (let [[condition _]
        (parse-comparison
         (butlast (rest tokens)))]

    condition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn collect-if-chain
  [lines start-index parent-indent]

  (loop [j start-index
         elifs []
         else-body []]

    (if (>= j (count lines))

      {:elifs elifs
       :else-body else-body
       :next-index j}

      (let [line   (nth lines j)
            tokens (vec (remove-comments line))]

        (cond

          ;; ---------------- ELIF ----------------
          (and (seq tokens)
               (= "elif" (:value (first tokens)))
               (= (indent-of line) parent-indent))

          (let [condition (parse-if-header tokens)
                [body-lines next-j]
                (collect-block lines (inc j) parent-indent)
                body (mapv parse-statement body-lines)]

            (recur next-j
                   (conj elifs
                         {:condition condition
                          :body body})
                   else-body))

          ;; ---------------- ELSE ----------------
          (and (seq tokens)
               (= "else" (:value (first tokens)))
               (= (indent-of line) parent-indent))

          (let [[body-lines next-j]
                (collect-block lines (inc j) parent-indent)
                body (mapv parse-statement body-lines)]

            {:elifs elifs
             :else-body body
             :next-index next-j})

          ;; ---------------- END CHAIN ----------------
          :else
          {:elifs elifs
           :else-body else-body
           :next-index j})))))

(defn parse-program [filename]

  (let [lines
        (vec (lex/tokenize-file filename))]
        (println "TOTAL LINES =" (count lines))

    (loop [i 0
           ast []]

      (if (>= i (count lines))

        ast

        (let [line   (nth lines i)
              tokens (vec (remove-comments line))]

          (println "INDEX:" i)
          (println "TOKENS:" tokens)

          (cond

            ;; skip empty/comment lines
            (empty? tokens)
            (recur (inc i) ast)

            ;; FOR
            (= "for"
               (:value (first tokens)))

            (let [{:keys [variable limit]}
                  (parse-for-header tokens)

                  [body-lines next-index]
                  (collect-block
                   lines
                   (inc i)
                   (indent-of line))

                  body
                  (mapv parse-statement body-lines)]

              (recur
               next-index
               (conj ast
                     (for-node
                      variable
                      limit
                      body))))

            ;; WHILE
            (= "while"
               (:value (first tokens)))

            (let [condition
                  (parse-while-header tokens)

                  [body-lines next-index]
                  (collect-block
                   lines
                   (inc i)
                   (indent-of line))

                  body
                  (mapv parse-statement body-lines)]

              (recur
               next-index
               (conj ast
                     (while-node
                      condition
                      body))))

            ;; IF
(= "if" (:value (first tokens)))

(let [condition (parse-if-header tokens)]

  (println "ENTERING IF")
  (println "CONDITION =" condition)

  (let [[then-lines next-index]
        (collect-block lines (inc i) (indent-of line))

        then-body
        (mapv parse-statement then-lines)

        chain
        (collect-if-chain lines next-index (indent-of line))

        {:keys [elifs else-body next-index]} chain]

    (println "RECURRING TO INDEX =" next-index)

    (recur next-index
           (conj ast
                 (if-node condition then-body elifs else-body)))))

            ;; everything else
            :else

            (recur
             (inc i)
             (conj ast
                   (parse-statement tokens)))))))))



(parse-program "Sample_Code.py")