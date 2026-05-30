#lang racket
(require "lexer.rkt")
(require "syntax_lexems_checker.rkt")




(check-syntax "Sample_Code.py")

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

;Assuming the syntax is correct
(define (evaluate_expr line variables)
  ;(displayln (car line))
  (cond
    [(token-type? (car line) 'NUMBER)
    ;(displayln(+ (str_to_int (car line)) 5))
    (cond
      [(or (null? (cdr line)) (token-type-and-value? (cadr line) 'DELIMITER ")"))
       ;(displayln (str_to_int(car line)))
       (str_to_int(car line))
       ]
      [else
       ;(define operator (token-value (cadr line)))
       (operate_expr (token-value (cadr line))
                     (str_to_int (car line))
                     (evaluate_expr (cddr line) variables)
                     variables)
       ]
      )]

  [(token-type? (car line) 'IDENTIFIER)
    (cond
      [(or (null? (cdr line)) (token-type-and-value? (cadr line) 'DELIMITER ")"))
       (get_var_value (car line) variables)
       ]
      [else
       (operate_expr (token-value (cadr line))
                     (get_var_value (car line) variables)
                     (evaluate_expr (cddr line) variables)
                     variables)
       ]
      )]

  [(token-type-and-value? (car line) 'DELIMITER "(")
   (evaluate_expr (cdr line) variables)
   ]

  [(token-type? (car line) 'STRING)
   (token-value (car line))
   ]
      )
  )

;Checks if the line is a variable assignement and sets the variable to its value if yes
(define (check_var_assign line variables)
  ;Sets the variable of the python file into the list of all variables
   (define (set_var var_name value)              
         (hash-set! variables var_name value)
    )
 
  (when
      (and (token-type? (car line) 'IDENTIFIER)
           (token-type? (cadr line) 'ASSIGNEMENT_OPERATOR))
    (define assign_op (token-value (cadr line)))
    (cond
      [(equal? assign_op "=")
       (set_var (token-value (car line)) (evaluate_expr (cddr line) variables))
       ]
      [(equal? assign_op "+=")
       (set_var (token-value (car line))
                (+ (get_var_value (car line) variables)
                   (evaluate_expr (cddr line) variables)))
       ]
      [(equal? assign_op "-=")
       (set_var (token-value (car line))
                (- (get_var_value (car line) variables)
                   (evaluate_expr (cddr line) variables)))
       ]
      [(equal? assign_op "*=")
       (set_var (token-value (car line))
                (* (get_var_value (car line) variables)
                   (evaluate_expr (cddr line) variables)))
       ]
      [(equal? assign_op "/=")
       (set_var (token-value (car line))
                (/ (get_var_value (car line) variables)
                   (evaluate_expr (cddr line) variables)))
       ]
      
    )
      )
  variables
  )

;Checks if the line is the call to the print function and displays the result if yes
(define (check_print line variables)
  (when (token-type-and-value? (car line) 'BUILTIN "print")
    (display (format "[Line ~a] print " (token-line (car line))))
    (displayln (evaluate_expr (cddr line) variables))
    )
  )

(define (execute_code file)
   ;List of all lines of tokens without any comments
   (define tokens (map (lambda (line) (filter is-not-comment? line)) (all_tokens file)))
   (define variables (make-hash))

   

   (for ([line (in-list tokens)])
     (unless (line-empty? line)
       (set! variables (check_var_assign line variables))
       (check_print line variables)
       )
     )
  variables
  )

;TESTING
;------------------------

(execute_code "Sample_Code.py")




