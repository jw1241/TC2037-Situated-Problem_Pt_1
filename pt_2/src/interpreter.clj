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
        "==" (= l r)))))

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

  (case (:type node)

    :assignment
    (exec-assignment node env)

    :print
    (exec-print node env)

    env))

(defn run-program [ast]

  (reduce
    (fn [env node]
      (execute-node node env))
    {}
    ast))