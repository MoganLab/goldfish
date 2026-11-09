(define (foo)  ;; matched
  (display "Hello"))

(define (bar  ;; unmatched open
  (define (baz)))  ;; extra close