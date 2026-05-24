#lang racket

(require "lexer.rkt")
(require "syntax_lexems_checker.rkt")

#|(displayln "Sample_Code")
(displayln (all_tokens "Sample_Code.py"))

(check-syntax "Sample_Code.py")
|#

;(displayln "Sample_Bad")
;(displayln (all_tokens "Sample_Bad.py"))

(check-syntax "Sample_Bad.py")
;(displayln "\nFILE 2")
;(displayln (all_tokens "file2.py"))