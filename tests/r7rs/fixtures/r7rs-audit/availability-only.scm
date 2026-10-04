(define-library (r7rs-audit availability-only)
  (import (scheme base))
  (begin (error "availability lookup must not execute this library")))
