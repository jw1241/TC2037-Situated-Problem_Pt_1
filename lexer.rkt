#lang racket
; Read the txt file
(define file "Sample_Code.txt")
(define in (open-input-file file))
(define list (sequence->list (in-lines in)))
(close-input-port in)

(define conditions '("if" "elif" "else"))
(define keywords '("def" "in" "return" "import" "True" "False"))
(define logical_operators '("and" "or" "not"))
(define comparison_operators '("==" "!=" ">" "<" ">=" "<="))
(define loops '("while" "for"))
(define operations '("=" "**" "*" "/" "+" "-" "math.sqrt")) ;The '=' sign should probably be somewhere else
(define delimiters '("(" ")" "," ":"))
(define builtins '("range" "len"))

; Create token structure and display actual values
(struct token (type value) #:transparent)

; Creates a list of all words within a line without whitespaces
(define (split_line line)
  (regexp-match* #px"[A-Za-z_][A-Za-z0-9_]*|\\d+|==|!=|<=|>=|\\*\\*|[:(),=+\\-*/<>]"
                 line))

; Classify the word type
(define (classify word)
  (cond
    [(member word keywords)
     (token 'KEYWORD word)]

    [(member word operations)
     (token 'OPERATION word)]

    [(member word delimiters)
     (token 'DELIMITER word)]

    [(member word conditions)
     (token 'CONDITION word)]

    [(member word logical_operators)
     (token 'LOGICAL_OPERATOR word)]

    [(member word comparison_operators)
     (token 'COMPARISON_OPERATOR word)]

    [(member word loops)
     (token 'LOOP word)]

    [(regexp-match #px"^-?[0-9]+$" word) ; Handles negative & positive numbers
     (token 'NUMBER word)]

    [(regexp-match #px"^[A-Za-z_][A-Za-z0-9_]*$" word) ; Identifiers
     (token 'IDENTIFIER word)]

    [else
     (token 'UNKNOWN word)]))


; Tokenize the entire file
(define (tokenize_line line)
  ;(displayln line)
  (cond
    [(regexp-match #px"^\\s*#" line) ;Classifying all comments
     (token 'COMMENT line)
    ]
    [else
     (map classify (split_line line)) ]
  )
)

(define all_tokens
  (map tokenize_line list))


;TESTING
;--------------------------
;(for-each displayln list)
all_tokens

#|
(car list)
(split_line (car list))
|#

