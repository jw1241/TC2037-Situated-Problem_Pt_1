#lang racket
(require "lexer.rkt")

(provide
 execute_code)
 

;Checks if the line is empty
(define (line-empty? line) 
  (null? line)
  )

;Checks if the token is a comment
(define (is-not-comment? token)
  (not(token-type? token 'COMMENT))
  )

;Checks if the token is of the type specified
(define (token-type? token type)
  (equal? (token-type token) type)
  )

;Checks if the token is of the value specified
(define (token-value? token value)
  (equal? (token-value token) value)
  )

;Checks if the token is of the type and value specified
(define (token-type-and-value? token type value)
  (and (token-type? token type) (token-value? token value))
  )

;Convert the value of the token into a number
(define (str_to_int token)
   (string->number (token-value token)) 
  )

;Get the value of a defined variable
(define (get_var_value token variables)
  (hash-ref variables (token-value token)
            (format "[ERROR] No value for key: ~a" (token-value token)))
  )


;Determine if we are evaluating a whole expression with parenthesis and such as a whole
(define (parse-factor tokens variables)

  (cond
    [(token-type? (car tokens) 'NUMBER)
     (values (str_to_int (car tokens))
             (cdr tokens))
     ]

    [(token-type? (car tokens) 'IDENTIFIER)
     (values (get_var_value (car tokens) variables)
             (cdr tokens))
     ]

    [(token-type? (car tokens) 'STRING)
     (values (token-value (car tokens))
             (cdr tokens))
     ]

    [(token-type-and-value? (car tokens) 'BUILTINS "len")

     (define arg-token
       (list-ref tokens 2))

     (define lst
       (get_var_value arg-token variables))

     (values
      (length lst)
      (list-tail tokens 5))
     ]

    [(token-type-and-value? (car tokens) 'DELIMITER "[")

     (define (collect-items toks acc)

       (cond
         [(token-type-and-value? (car toks) 'DELIMITER "]")
          (values (reverse acc)
              (cdr toks))
          ]

         [else
          (define-values (value rest)
            (parse-expression toks variables))

          (define next-rest
            (if (and (pair? rest)
                     (token-type-and-value? (car rest)
                                            'DELIMITER ","))
                (cdr rest)
                rest))

          (collect-items next-rest
                         (cons value acc))
          ]
         )
       )

      (collect-items (cdr tokens) '())
     ]

    [(token-type-and-value? (car tokens) 'DELIMITER "(")

     (define-values (value rest)
       (parse-expression (cdr tokens) variables))

     (values value
             (cdr rest))]

    [(and (token-type-and-value? (car tokens) 'KEYWORD "math")
          (token-type-and-value? (cadr tokens) 'DELIMITER ".")
          (token-type-and-value? (caddr tokens) 'IDENTIFIER "sqrt"))

     (define-values (arg rest)
       (parse-expression (cddddr tokens) variables))

     (values (sqrt arg)
             (cdr rest))]

    [else
     (error "parse-factor: unexpected token"
            (car tokens))
     ]
    )
  )

;Handles exponents in PEMDAS
(define (parse-power tokens variables)

  (define-values (left rest)
    (parse-factor tokens variables))

  (if (and (pair? rest)
           (token-type-and-value? (car rest) 'OPERATION "**"))

      (let-values ([(right rest2)
                    (parse-power (cdr rest) variables)]
                   )
        (values (expt left right)
                rest2))

      (values left rest))
  )

;Handles mutliplication and division in PEMDAS
(define (parse-term tokens variables)

  (define-values (left rest)
    (parse-power tokens variables))

  (let loop ([left left]
             [rest rest])

    (if (and (pair? rest)
             (member (token-value (car rest))
                     '("*" "/"))
             )

        (let* ([op (token-value (car rest))
                   ]
               )

          (define-values (right rest2)
            (parse-power (cdr rest) variables))

          (loop
           (if (equal? op "*")
               (* left right)
               (/ left right))
           rest2))

        (values left rest))
    )
  )

;Determines expressions like 12 + 4 
(define (parse-expression tokens variables)

  (define-values (left rest)
    (parse-term tokens variables))

  (let loop ([left left]
             [rest rest])

    (if (and (pair? rest)
             (member (token-value (car rest))
                     '("+" "-"))
             )

        (let* ([op (token-value (car rest))
                   ]
               )

          (define-values (right rest2)
            (parse-term (cdr rest) variables))

          (loop
           (if (equal? op "+")
               (+ left right)
               (- left right))
           rest2))

        (values left rest))
    )
  )

;Determine what operation to do in EMDAS
(define (operate_expr operator value1 value2 variables)
  (cond
    [(equal? operator "+")
     (+ value1 value2)
     ]
    [(equal? operator "-")
     (- value1 value2)
     ]
    [(equal? operator "*")
     (* value1 value2)
     ]
    [(equal? operator "/")
     (/ value1 value2)
     ]
    [(equal? operator "**")
     (expt value1 value2)
     ]
    [else
     value1
     ]
    )
  )

;Condition Evaluator
(define (evaluate-condition line variables)
  (define left
    (if (token-type? (cadr line) 'IDENTIFIER)
        (get_var_value (cadr line) variables)
        (str_to_int (cadr line))
        )
    )

  (define op (token-value (caddr line))
    )

  (define right
    (if (token-type? (cadddr line) 'IDENTIFIER)
        (get_var_value (cadddr line) variables)
        (str_to_int (cadddr line))
        )
    )

  (cond
    [(equal? op ">")  (> left right)]
    [(equal? op "<")  (< left right)]
    [(equal? op "==") (= left right)]
    [(equal? op "!=") (not (= left right))]
    [(equal? op ">=") (>= left right)]
    [(equal? op "<=") (<= left right)]
    )
  )

(define (evaluate_expr line variables)

  (define-values (result remaining)
    (parse-expression line variables))
  
  result)

;Checks if the line is a variable assignement and sets the variable to its value if yes
(define (check_var_assign line variables variable-history)
  ;Sets the variable of the python file into the list of all variables
   (define (set_var var_name value)

  ;; current value
  (hash-set! variables var_name value)

  ;; history
  (hash-update!
   variable-history
   var_name
   (lambda (lst) (append lst (list value))
     )'()
   )
     )
 
  (when
      (and (token-type? (car line) 'IDENTIFIER)
           (token-type? (cadr line) 'ASSIGNEMENT_OPERATOR))
    (define assign_op (token-value (cadr line))
      )
    (cond
      [(equal? assign_op "=")
       (set_var (token-value (car line)) (evaluate_expr (cddr line) variables))
       ]
      [(equal? assign_op "+=")
       (set_var (token-value (car line))
                (+ (get_var_value (car line) variables)
                   (evaluate_expr (cddr line) variables))
                )
       ]
      [(equal? assign_op "-=")
       (set_var (token-value (car line))
                (- (get_var_value (car line) variables)
                   (evaluate_expr (cddr line) variables))
                )
       ]
      [(equal? assign_op "*=")
       (set_var (token-value (car line))
                (* (get_var_value (car line) variables)
                   (evaluate_expr (cddr line) variables))
                )
       ]
      [(equal? assign_op "/=")
       (set_var (token-value (car line))
                (/ (get_var_value (car line) variables)
                   (evaluate_expr (cddr line) variables))
                )
       ]
      
    )
      )
  variables
  )


;Checks if the line is the call to the print function and displays the result if yes
(define (check_print line variables must-print)
  (when (and (token-type-and-value? (car line) 'BUILTINS "print")
             (token-type? (caddr line) 'IDENTIFIER))

    (hash-set! must-print
               (token-value (caddr line))
               #t))

  must-print)



;Execute to check if we we are supposed to print something or not
(define (execute-line line variables variable-history must-print)


  (check_var_assign line variables variable-history)
  (check_print line variables must-print))

(define (collect-body tokens start-index parent-indent)

  (define body '())
  (define i start-index)

  (let loop ()

    (when (and (< i (length tokens))
               (> (token-indent (car (list-ref tokens i))
                                )
                  parent-indent))

      (set! body
            (append body
                    (list (list-ref tokens i))
                    )
            )

      (set! i (+ i 1))

      (loop))
    )

  (values body i))

;Interpreter for 'for' loops
(define (execute-for for-line body variables variable-history must-print)

  (define loop-var
    (token-value (cadr for-line))
    )

  (define limit
    (str_to_int (list-ref for-line 5))
    )

  (for ([i (in-range limit)]
        )

    (hash-set! variables loop-var i)

    (hash-update!
     variable-history
     loop-var
     (lambda (lst) (append lst (list i))
       )
     '())

    (for ([line body])
      (execute-line
       line
       variables
       variable-history
       must-print))
    )
  )

;Interpreter for while loops
(define (execute-while while-line body variables variable-history must-print)
  (when (evaluate-condition while-line variables)

    (for ([line body])
      
      (execute-line line variables variable-history must-print))

    (execute-while while-line body variables variable-history must-print))
  )

;Interprert the python
(define (execute_code file)

  (define variables (make-hash))
  (define variable-history (make-hash))

  (define tokens
  (filter
   (lambda (line) (not (null? line))
     )
   (map (lambda (line)
          (filter is-not-comment? line)
          )
        (all_tokens file))
   )
    )

  (define must-print (make-hash))

  (define i 0)

  (let loop ()

    (when (< i (length tokens))

      (define line (list-ref tokens i))

      (unless (line-empty? line)

        (cond

          [(token-type-and-value? (car line) 'LOOP "for")

           (define-values (body next-i)
             (collect-body
              tokens
              (+ i 1)
              (token-indent (car line))))

           (execute-for
            line
            body
            variables
            variable-history
            must-print)

           (set! i (- next-i 1))]

          ;; WHILE LOOP
          [(token-type-and-value? (car line) 'LOOP "while")

           (define-values (body next-i)
             (collect-body
              tokens
              (+ i 1)
              (token-indent (car line))))

           (execute-while
            line
            body
            variables
            variable-history   ; <-- added
            must-print)

           (set! i (- next-i 1))]

          ;; NORMAL LINE
          [else
           (execute-line
            line
            variables
            variable-history   ; <-- added
            must-print)]))

      (set! i (+ i 1))
      (loop)))

  ;; return history for debugging
  variable-history)

;TESTING
;------------------------

;(execute_code "Sample_Code.py")




