(define-library (analysis second)
  (import (scheme base))
  (export shared-name)
  (begin
    (define (shared-name x y) #t)
  ) ;begin
) ;define-library
