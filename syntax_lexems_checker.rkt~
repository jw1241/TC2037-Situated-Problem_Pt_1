#lang racket

(require "lexer.rkt")

; Check if string is empty, only spaces, only tabs/newlines
(define (string-blank? s)
  (regexp-match? #px"^\\s*$" s))

(define keywords
  '("if" "elif" "else"
    "while" "for" "def"))

(define (check-colon line line-num)

  (define trimmed
    (string-trim line))

  (when
      (or
       (string-prefix? trimmed "if")
       (string-prefix? trimmed "elif")
       (string-prefix? trimmed "else")
       (string-prefix? trimmed "while")
       (string-prefix? trimmed "for")
       (string-prefix? trimmed "def"))

    (unless (string-suffix? trimmed ":")

      (error
       (format
        "Syntax Error [Line ~a]: Missing ':'"
        line-num)))))

(define (balanced-parens? line)

  (= (count (lambda (c) (char=? c #\())
             (string->list line))

     (count (lambda (c) (char=? c #\)))
             (string->list line))))

(define (check-parens line line-num)

  (unless (balanced-parens? line)

    (error
     (format
      "Syntax Error [Line ~a]: Unbalanced parentheses"
      line-num))))

(define (check-indentation line line-num)

  (define spaces
    (indent-level line))

  (unless (= (remainder spaces 4) 0)

    (error
     (format
      "Indentation Error [Line ~a]"
      line-num))))

(define (check-print line line-num)

  (when (regexp-match #px"print" line)

    (unless
        (regexp-match #px"print\\(.+\\)" line)

      (error
       (format
        "Syntax Error [Line ~a]: Invalid print syntax"
        line-num)))))


(define (check-unknowns token-lines)

  (for ([line token-lines])

    (for ([tk line])

      (when (eq? (token-type tk) 'UNKNOWN)

        (error
         (format
          "Unknown token '~a' on line ~a"
          (token-value tk)
          (token-line tk)))))))

(define (check-syntax filename)

  (define lines
    (read-file-lines filename))

  (define token-lines
    (all_tokens filename))

  ; Token validation
  (check-unknowns token-lines)

  ; Line-by-line syntax validation
  (for ([line lines]
        [line-num (in-naturals 1)])

    (unless (string-blank? line)

      (check-colon line line-num)

      (check-parens line line-num)

      (check-indentation line line-num)

      (check-print line line-num))))