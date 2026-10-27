(define-library (liii arity)
  (import (scheme base) (scheme case-lambda))
  (export f1 f2)
  (begin

    (define (f1 a b) #t)

    (define f2
      (case-lambda
       ((a) #f)
       ((a b) #t)
      ) ;case-lambda
    ) ;define
  ) ;begin
) ;define-library
