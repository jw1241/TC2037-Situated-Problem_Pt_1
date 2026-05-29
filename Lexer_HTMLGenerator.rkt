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
 read-file-lines
 escape-html-chars
 token->html
 line->html
 tokens->html-string
 write-html-file)

(define conditions '("if" "elif" "else"))
(define assignment_operators '("=" "-=" "+=" "*=" "/=" "%="))
(define keywords '("def" "in" "return" "import" "True" "False"))
(define logical_operators '("and" "or" "not"))
(define comparison_operators '("==" "!=" ">" "<" ">=" "<="))
(define loops '("while" "for"))
(define operations '("**" "*" "/" "%" "+" "-" "math.sqrt"))
(define delimiters '("(" ")" "," ":"))
(define builtins '("range" "len"))


(struct token (type value line indent) #:transparent)

(define (read-file-lines filename)
  (define in (open-input-file filename))
  (define lines (sequence->list (in-lines in)))
  (close-input-port in)
  lines)

(define (indent-level line)
  (- (string-length line)
     (string-length
      (string-trim line #:left? #t))))

(define (split_line line)
  (regexp-match*
   #px"\"[^\"]*\"|[A-Za-z_][A-Za-z0-9_]*|-?\\d+(\\.\\d+)?|==|!=|<=|>=|\\+=|-=|\\*=|/=|\\*\\*|[ \t]+|[:(),=+\\-*/<>]|[^ \t]"
   line))

(define (classify word line-num indent)
  (cond
    [(member word keywords)
     (token 'KEYWORD word line-num indent)]
    
    [(member word builtins)
     (token 'BUILTIN word line-num indent)]
    
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

    [(regexp-match #px"^[ \t]+$" word)
     (token 'WHITESPACE word line-num indent)]

    [(regexp-match #px"^-?[0-9]+$" word) 
     (token 'NUMBER word line-num indent)]

    [(regexp-match #px"^\"[^\"]*\"$" word)
     (token 'STRING word line-num indent)]

    [(regexp-match #px"^[A-Za-z_][A-Za-z0-9_]*$" word) 
     (token 'IDENTIFIER word line-num indent)]

    [else
     (token 'UNKNOWN word line-num indent)]))

(define (tokenize_line line line-num)
  (define indent (indent-level line))
  (define comment-match (regexp-match-positions #px"#" line))
  
  (cond
    [(regexp-match #px"^\\s*#" line) 
     (list (token 'COMMENT line line-num indent))]

    [comment-match
     (define comment-pos (caar comment-match))
     (define code-part (substring line 0 comment-pos))
     (define comment-part (substring line comment-pos))
     (append
      (map (lambda (a) (classify a line-num indent)) (split_line code-part))
      (list (token 'COMMENT comment-part line-num indent)))]

    [else
     (map (lambda (a) (classify a line-num indent)) (split_line line))]))

(define (all_tokens filename)
  (define lines (read-file-lines filename))
  (for/list ([line lines]
             [line-num (in-naturals 1)])
    (tokenize_line line line-num)))


; HTML
(define (escape-html-chars str)
  (string-replace 
   (string-replace 
    (string-replace str "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))

(define (token->html tok)
  (define type-str (string-downcase (symbol->string (token-type tok))))
  (define safe-value (escape-html-chars (token-value tok)))
  (cond
    [(eq? (token-type tok) 'WHITESPACE) 
     (token-value tok)] 
    [else 
     (format "<text class=\"~a\">~a</text>" type-str safe-value)]))

(define (line->html line-tokens)
  (string-append (string-join (map token->html line-tokens) "") "<br>\n"))

(define (tokens->html-string nested-tokens)
  (string-join (map line->html nested-tokens) ""))

;CSS /HTML
(define (write-html-file source-py-file output-html-file)
  (define program-tokens (all_tokens source-py-file))
  (define highlighted-body (tokens->html-string program-tokens))
  
  (with-output-to-file output-html-file
    (lambda ()
      (displayln "<!DOCTYPE html>")
      (displayln "<html>")
      (define conditions '("if" "elif" "else"))
      (displayln "<head>")
      (displayln "<meta charset=\"utf-8\">")
      (displayln "<title>Syntax Highlighted Output</title>")
      (displayln "<style>")
      
      (displayln "text { font-family: 'Monokai', monospace; font-weight: bold; white-space: pre; }")
      (displayln "body { background-color: #272822; padding: 20px; line-height: 1.5; }")
      (displayln ".comment { color: gray; font-style: italic; }")
      (displayln ".number { color: #8f00ff; }")
      (displayln ".keyword { color: #f92672; }")
      (displayln ".condition { color: #ff9800; }")
      (displayln ".loop { color: #ae81ff; }")
      (displayln ".string { color: #e6db74; }")
      (displayln ".identifier { color: #ffffff; }")
      (displayln ".delimiter { color: #a6e22e; }")
      (displayln ".assignement_operator { color: #f8f8f2; }")
      (displayln ".operation { color: #66d9ef; }")
      (displayln ".logical_operator { color: #fd971f; }")
      (displayln ".comparison_operator { color: #66d9ef; }")
      (displayln ".builtin { color: burlywood; }")
      (displayln ".whitespace { color: white; }")
      (displayln ".unknown { color: #ff0000; text-decoration: underline; }")
      
      (displayln "</style>")
      (displayln "</head>")
      (displayln "<body>")
      (displayln highlighted-body)
      (displayln "</body>")
      (displayln "</html>"))
    #:exists 'replace))

(write-html-file "Sample_Code.py" "Highlighter_Output.html")