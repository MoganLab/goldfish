(define-library (liii union-dep)
  (import (scheme base))
  (export my-take my-opts)
  (begin

    (define (my-take x) x)

    (define (my-opts a b) a)
  ) ;begin
) ;define-library
