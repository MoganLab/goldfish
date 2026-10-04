(define-library (r7rs-audit consumer)
  (import (scheme base) (r7rs-audit provider) (r7rs-audit state))
  (export initial)
  (begin (record! 'consumer) (define initial (hygienic-plus 2))))
