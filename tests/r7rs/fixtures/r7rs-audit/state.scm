(define-library (r7rs-audit state)
  (import (scheme base))
  (export record! events)
  (begin
    (define log '())
    (define (record! event) (set! log (cons event log)))
    (define (events) (reverse log))))
