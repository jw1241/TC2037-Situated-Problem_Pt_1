(ns interpreter
  (:require 
    [parser :as par]
    [lexer :as lex]
    [clojure.string :as str]
    )
  )


(defn pow-n [x n]
   (reduce * (repeat n x))
)

(defn eval-expression [expr env]
  
  (case (:type expr)

    :number
    (:value expr)

    :identifier
    (get env (:name expr))

    :binary-op

    (let [l (eval-expression (:left expr) env)
          r (eval-expression (:right expr) env)]

      (case (:operator expr)

        "+" (+ l r)
        "-" (- l r)
        "*" (* l r)
        "/" (/ l r)
        "==" (= l r)
        "**" (pow-n l r)
        ))

    :string    
    (:value expr)
    
  ))
        

(defn exec-assignment [node env]

  (assoc env
         (:variable node)
         (eval-expression (:value node)
                          env)))

(defn exec-print [node env]

  (println
    (eval-expression
      (:expression node)
      env))

  env)

(defn execute-node [node env]
  ;(println node)
  ;(println env)
  (case (:type node)

    :assignment
    (exec-assignment node env)

    :print
    (exec-print node env)

    env)
  
)

(defn run-program [filename]
  (reduce
    (fn [env node]
      (execute-node node env))

    {}  ;Set of variables
    (par/parse-program filename) ;tokens
  ))


(run-program "Sample_Code.py")