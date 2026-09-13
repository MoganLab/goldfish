(define-library (m2a-libC)
  (import (scheme base))
  (export c-add c-mul c-tag)
  (begin
    (define (c-add a b) (+ a b))
    (define (c-mul a b) (* a b))
    (define c-tag "C")))
