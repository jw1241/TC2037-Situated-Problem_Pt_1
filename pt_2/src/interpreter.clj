(ns interpreter
  (:require
   [parser :as par]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATH HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pow-n [x n]
  (reduce * (repeat n x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPRESSION EVALUATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare execute-node)

(defn eval-expression [expr env]

  (when (nil? expr)
    (throw
     (ex-info
      "Expression is nil"
      {:env env})))

  (case (:type expr)

    ;; NUMBER

    :number
    (:value expr)

    ;; STRING

    :string
    (:value expr)

    ;; VARIABLE

    :identifier
    (get env (:name expr))

    ;; LIST

    :list
    (mapv
     #(eval-expression % env)
     (:elements expr))

    ;; COMPARISONS

    :comparison

    (let [l (eval-expression (:left expr) env)
          r (eval-expression (:right expr) env)]

      (case (:operator expr)

        "==" (= l r)
        ">"  (> l r)
        "<"  (< l r)

        (throw
         (ex-info
          "Unknown comparison operator"
          {:operator (:operator expr)}))))

    ;; FUNCTION CALLS

    :call

    (case (:name expr)

      "math.sqrt"
      (Math/sqrt
       (double
        (eval-expression
         (first (:args expr))
         env)))

      (throw
       (ex-info
        "Unknown function"
        {:function (:name expr)})))

    ;; BINARY OPERATIONS

    :binary-op

    (let [l (eval-expression (:left expr) env)
          r (eval-expression (:right expr) env)]

      (case (:operator expr)

        "+" (+ l r)
        "-" (- l r)
        "*" (* l r)
        "/" (/ l r)
        "**" (pow-n l r)

        (throw
         (ex-info
          "Unknown operator"
          {:operator (:operator expr)}))))

    (throw
     (ex-info
      "Unknown expression type"
      {:expr expr}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASSIGNMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exec-assignment [node env]

  (assoc
   env
   (:variable node)
   (eval-expression
    (:value node)
    env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -=
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exec-minus-assignment [node env]

  (update
   env
   (:variable node)

   #(- %
       (eval-expression
        (:value node)
        env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exec-print [node env]

  (println

   (eval-expression
    (:expression node)
    env))

  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOR LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exec-for [node env]

  (reduce

   (fn [current-env i]

     (let [loop-env
           (assoc current-env
                  (:variable node)
                  i)]

       (reduce
        execute-node
        loop-env
        (:body node))))

   env

   (range (:limit node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHILE LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exec-while [node env]

  (loop [current-env env]

    (if

     (eval-expression
      (:condition node)
      current-env)

      (recur

       (reduce
        execute-node
        current-env
        (:body node)))

      current-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exec-if [node env]

  (if

   (eval-expression
    (:condition node)
    env)

    (reduce
     execute-node
     env
     (:then node))

    env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NODE EXECUTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn execute-node [env node]

  (case (:type node)

    :assignment
    (exec-assignment node env)

    :minus-assignment
    (exec-minus-assignment node env)

    :print
    (exec-print node env)

    :for
    (exec-for node env)

    :while
    (exec-while node env)

    :if
    (exec-if node env)

    ;; ignore imports

    :import
    env

    (do
      (println "Unknown node:" node)
      env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-program [filename]

  (reduce

   execute-node

   {}

   (par/parse-program filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENTRY POINT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-program "Sample_Code.py")