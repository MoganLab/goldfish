(define-library (m2a-libB)
  (import (scheme base) (m2a-libC))
  (export b-combine b-describe)
  (begin
    (define (b-combine x y) (c-add (c-mul x y) 100))
    (define (b-describe x) (string-append c-tag ":" (number->string x)))))
