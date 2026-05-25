#lang racket

(provide
 token
 token-type
 token-value
 token-line
 token-indent
 all_tokens
 tokenize_line
 classify
 indent-level
 read-file-lines)

; Read the txt file
(define (read-file-lines filename)
  (define in (open-input-file filename))
  (define lines (sequence->list (in-lines in)))
  (close-input-port in)
  lines)

(define conditions '("if" "elif" "else"))
(define assignment_operators '("=" "-=" "+=" "*=" "/=" "%="))
(define keywords '("def" "in" "return" "import" "True" "False"))
(define logical_operators '("and" "or" "not"))
(define comparison_operators '("==" "!=" ">" "<" ">=" "<="))
(define loops '("while" "for"))
(define operations '("**" "*" "/" "%" "+" "-" "math.sqrt")) ;The '=' sign should probably be somewhere else
(define delimiters '("(" ")" "," ":"))
(define builtins '("range" "len"))

; Create token structure and display actual values
(struct token (type value line indent) #:transparent)

(define (indent-level line)
  (- (string-length line)
     (string-length
      (string-trim line #:left? #t))))

; Creates a list of all words within a line without whitespaces
(define (split_line line)
  (regexp-match*
   #px"\"[^\"]*\"|[A-Za-z_][A-Za-z0-9_]*|-?\\d+(\\.\\d+)?|==|!=|<=|>=|\\+=|-=|\\*=|/=|\\*\\*|[:(),=+\\-*/<>]|[^ \t]"
   line)
)

; Classify the word type
(define (classify word line-num indent)
  (cond
    [(member word keywords)
     (token 'KEYWORD word line-num indent)]
    
    [(member word assignment_operators)
     (token 'ASSIGNEMENT_OPERATOR word line-num indent)]

    [(member word operations)
     (token 'OPERATION word line-num indent)]

    [(member word delimiters)
     (token 'DELIMITER word line-num indent)]

    [(member word conditions)
     (token 'CONDITION word line-num indent)]

    [(member word logical_operators)
     (token 'LOGICAL_OPERATOR word line-num indent)]

    [(member word comparison_operators)
     (token 'COMPARISON_OPERATOR word line-num indent)]

    [(member word loops)
     (token 'LOOP word line-num indent)]

    [(regexp-match #px"^-?[0-9]+$" word) ; Handles negative & positive numbers
     (token 'NUMBER word line-num indent)]

    [(regexp-match #px"^\"[^\"]*\"$" word)
     (token 'STRING word line-num indent)]

    [(regexp-match #px"^[A-Za-z_][A-Za-z0-9_]*$" word) ; Identifiers
     (token 'IDENTIFIER word line-num indent)]

    [else
     (token 'UNKNOWN word line-num indent)]))


; Tokenize the entire file
(define (tokenize_line line line-num)
  (define indent
  (indent-level line))
  
  (define comment-match
    (regexp-match-positions #px"#" line))
  ;(displayln line)
  (cond
    [(regexp-match #px"^\\s*#" line) ;Classifying all comments
     (list
      (token 'COMMENT line line-num indent)
      )]

     ; Line has code + comment
    [comment-match
     (define comment-pos
       (caar comment-match))

     (define code-part
       (substring line 0 comment-pos))

     (define comment-part
       (substring line comment-pos))

      (append
       (map
        (lambda (a)
          (classify a line-num indent))
        (split_line code-part))

        (list
       (token 'COMMENT comment-part line-num indent))
        )]

    ; Normal line
    [else
     (map
      (lambda (a)
        (classify a line-num indent))
      (split_line line))
    ]
    #|[else
     (map classify (split_line line)) ]
  |#
  )
)

(define (all_tokens filename)
  (define lines (read-file-lines filename))
  
  (for/list ([line lines]
             [line-num (in-naturals 1)])
    (tokenize_line line line-num)
  )
)
  ;(map tokenize_line list))


;TESTING
;--------------------------
(define good_file "Sample_Code.py")
;(map split_line (read-file-lines good_file))

(all_tokens good_file)
#|
(car list)
(split_line (car list))
|#

