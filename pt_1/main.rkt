#lang racket

(require "lexer.rkt")
(require "syntax_lexems_checker.rkt")
(require "interpreter.rkt")
(require "html-generator.rkt")
(require "execution_html_generator.rkt")

;Tokens from file with no syntax, lexicon, or semantic errors

;(displayln (all_tokens "Sample_Code.py"))
;(time (all_tokens "Sample_Code.py"))
(time (all_tokens-parallel "Sample_Code.py"))
;Tokens from file with no syntax, lexicon, or semantic errors

;(displayln (all_tokens "Sample_Bad.py"))

;Checking if there are any errors with the file

;(check-syntax "Sample_Code.py")6

;(check-syntax "Sample_Bad.py")



;Interpreter of the error free file

;(execute_code "Sample_Code.py")


;Syntax HTML Generator

;(run-syntax-highlighter "Sample_Code.py" "Highlighter_Output.html")

;(run-syntax-highlighter "Sample_Bad.py" "Highlighter_Output1.html")
;Execution Results

;(write-results-html (execute_code "Sample_Code.py") "results.html")







