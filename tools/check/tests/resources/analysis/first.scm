(define-library (analysis first)
  (import (scheme base))
  (export local-name shared-name optional-name rest-name keyword-name)
  (begin
    (define (local-name x) x)
    (define (shared-name x y) #t)
    (define* (optional-name x (y 2)) #t)
    (define (rest-name x . rest) rest)
    (define* (keyword-name x :allow-other-keys) #t)
  ) ;begin
) ;define-library
