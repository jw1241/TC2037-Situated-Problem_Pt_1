#lang racket
(define conditions '("if", "elif", "else"))
(define logical_opeators ("and", "or", "not"))
(define compairson_operators ("==", "!=", ">", "<"))
(define loops '("while", "for"))
(define operations '("(", ")", "**", "*", "/", "+", "-", "math.sqrt"))

(define (classify_type word)
  (cond
    [(regexp-match