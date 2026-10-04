(define-library (r7rs-audit conditional)
  (import (scheme base))
  (cond-expand
    (r7rs (export answer) (begin (define answer 42)))
    (else (export answer) (begin (define answer 0)))))
