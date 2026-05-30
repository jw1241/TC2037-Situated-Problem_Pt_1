#lang racket
(require "interpreter.rkt")

(provide write-results-html)

(define (write-results-html variable-history filename)

  (call-with-output-file
   filename
   #:exists 'replace

   (lambda (out)

     (displayln "<!DOCTYPE html>" out)
     (displayln "<html>" out)
     (displayln "<head>" out)
     (displayln "<title>Interpreter Results</title>" out)

     (displayln
      "<style>
        table { border-collapse: collapse; }
        th, td {
          border: 1px solid black;
          padding: 8px;
        }
      </style>"
      out)

     (displayln "</head>" out)
     (displayln "<body>" out)

     (displayln "<h1>Interpreter Results</h1>" out)

     (displayln "<table>" out)
     (displayln
      "<tr><th>Variable</th><th>Values</th></tr>"
      out)

     (for ([key (sort (hash-keys variable-history)
                      string<?)])

       (fprintf out
                "<tr><td>~a</td><td>~a</td></tr>\n"
                key
                (hash-ref variable-history key)))

     (displayln "</table>" out)

     (displayln "</body>" out)
     (displayln "</html>" out))
    )
  )
