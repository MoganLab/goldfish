(define-library (liii clean)
  (import (scheme base) (scheme case-lambda) (liii other))
  (export f1 f2 f3)
  (begin

    (define (f1 x) #t)

    (define f2
      (case-lambda
       ((a b) #f)
       ((a b c) #t)
      ) ;case-lambda
    ) ;define
  ) ;begin
) ;define-library
