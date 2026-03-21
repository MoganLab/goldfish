(define-library (demo hello)
  (export main)
  (import (scheme base))
  (begin
    (define (main)
      (display "Hello from (demo hello)!")
      (newline))
  )
)
