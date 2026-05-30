#lang racket

(require "lexer.rkt")

(provide check-syntax)

; Check if string is empty, only spaces, only tabs/newlines
(define (string-blank? s)
  (regexp-match? #px"^\\s*$" s))

(define keywords
  '("if" "elif" "else"
    "while" "for" "def"))

(define (check-keywords token-lines errors)

  (for ([line token-lines])

    (when (and (not (null? line))
               (eq? (token-type (car line)) 'IDENTIFIER))

      (define first-word
        (token-value (car line))
        )

      (define line-num
        (token-line (car line))
        )

      (when (member first-word '("whiel" "fro" "els"))

        (set! errors
              (cons
               (cons
                line-num
                (format
                 "Possible misspelled keyword '~a' on line ~a"
                 first-word
                 line-num))
               errors))
        )
      )
    )

  errors
  )

;Check if colons are missing
(define (check-colon line line-num errors)

  (define trimmed
    (string-trim line))

  (when
      (regexp-match?
       #px"^(if|elif|else|while|for|def)\\b"
       trimmed)

    (unless (string-suffix? trimmed ":")

      (set! errors
            (cons
             (cons line-num
                   (format
                    "Syntax Error [Line ~a]: Missing ':'"
                    line-num))
             errors))
      )
    )

  errors)

;Check if parentheses are balanced one open and one close
(define (balanced-parens? line)

  (= (count (lambda (c) (char=? c #\())
             (string->list line))

     (count (lambda (c) (char=? c #\)))
             (string->list line))
     )
  )

(define (check-parens line line-num errors)

  (unless (balanced-parens? line)

    (set! errors
          (cons
           (cons line-num
                 (format "Syntax Error [Line ~a]: Unbalanced parentheses"
                         line-num))
           errors)))

  errors)


; Check for proper indentation
(define (check-indentation lines errors)

  (define expected-indent 0)
  (define previous-ended-colon #f)

  (for ([line lines]
        [line-num (in-naturals 1)]
        )

    (unless (string-blank? line)

      (define current-indent
        (indent-level line))

      (define trimmed
        (string-trim line))

      ; Must be multiple of 4
      (unless (= (remainder current-indent 4) 0)

        (set! errors
              (cons
               (cons line-num
                     (format
                      "Indentation Error [Line ~a]"
                      line-num))
               errors)
              )
        )

      ; Expected indentation after colon
      (when previous-ended-colon

        (unless (> current-indent expected-indent)

          (set! errors
                (cons
                 (cons
                  line-num
                  (format
                   "Indentation Error [Line ~a]: Expected indented block"
                   line-num))
                 errors))
          )
        )
      )
    )
  errors
  )

;Check if print was spelled correctly
(define (check-print line line-num errors)

  (when (regexp-match #px"print" line)

    (unless
        (regexp-match #px"print\\(.+\\)" line)

      (set! errors
            (cons
             (cons
              line-num
              (format
               "Syntax Error [Line ~a]: Invalid print syntax"
               line-num))
             errors))
      )
    )

  errors
  )

;Return any unknown tokens
(define (check-unknowns token-lines errors)

  (for ([line token-lines])

    (for ([tk line])

      (when (eq? (token-type tk) 'UNKNOWN)

        (define line-num
          (token-line tk))

        (set! errors
              (cons
               (cons
                line-num
                (format
                 "Unknown token '~a' on line ~a"
                 (token-value tk)
                 line-num))
               errors))
        )
      )
    )

  errors)

;Check the file for errors
(define (check-syntax filename)

  (define lines
    (read-file-lines filename))

  (define token-lines
    (all_tokens filename))

  (define errors '())

  (for ([line lines]
        [line-num (in-naturals 1)]
        )

    (unless (string-blank? line)

      (set! errors
            (check-colon line line-num errors))

      (set! errors
            (check-parens line line-num errors))

      (set! errors
            (check-print line line-num errors))
      )
    )

  (set! errors
      (check-indentation lines errors))

  ; Check unknown tokens
  (set! errors
        (check-unknowns token-lines errors))
  
  ;Check unknown keywords
  (set! errors
      (check-keywords token-lines errors))


  ; Print all errors
  (if (null? errors)

    (displayln "No syntax errors found.")

    (let ([sorted-errors
       (sort errors < #:key car)]
          )

  (for-each
   (lambda (err)
     (displayln (cdr err))
     )
   sorted-errors))
    )
)