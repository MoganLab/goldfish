(define-library (r7rs-audit state-factory)
  (import (scheme base))
  (export define-state)
  (begin
    (define-syntax define-state
      (syntax-rules ()
        ((_ variable getter bump initial)
         (begin
           (define variable initial)
           (define (private-getter) variable)
           (define (getter) (private-getter))
           (define (bump) (set! variable (+ variable 1)))))))))
