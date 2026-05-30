#lang racket
(require "lexer.rkt")

(provide run-syntax-highlighter)

(define (escape-html-chars str)
  (string-replace 
   (string-replace 
    (string-replace str "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))


(define (token->html tok)
  (define type-str (string-downcase (symbol->string (token-type tok))
                                    )
    )
  (define safe-value (escape-html-chars (token-value tok))
    )
  (cond
    [(eq? (token-type tok) 'WHITESPACE) 
     (token-value tok)] ;
    [else 
     (format "<text class=\"~a\">~a</text>" type-str safe-value)]
    )
  )

(define (line->html line-tokens)
  (string-append (string-join (map token->html line-tokens) "") "<br>\n"))

(define (tokens->html-string nested-tokens)
  (string-join (map line->html nested-tokens) ""))

(define (generate-html-document program-tokens output-html-file)
  (define highlighted-body (tokens->html-string program-tokens))
  
  (with-output-to-file output-html-file
    (lambda ()
      (displayln "<!DOCTYPE html>")
      (displayln "<html>")
      (displayln "<head>")
      (displayln "<meta charset=\"utf-8\">")
      (displayln "<title>Syntax Highlighted Output</title>")
      (displayln "<style>")

      (displayln "text { font-family: 'Monokai', monospace; font-weight: bold;}")
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
      (displayln ".unknown { color: #ff0000; text-decoration: underline; }")

      (displayln "</style>")
      (displayln "</head>")
      (displayln "<body>")
      (displayln highlighted-body)
      (displayln "</body>")
      (displayln "</html>"))
    #:exists 'replace))

(define (run-syntax-highlighter python-input-file html-output-file)
  (define parsed-tokens (all_tokens python-input-file))
  (generate-html-document parsed-tokens html-output-file)
  (displayln "Successfully highlighted code!"))
