(import (srfi srfi-1)
  (srfi srfi-16)
  (srfi srfi-78)
) ;import

(check-set-mode! 'report-failed)

(define (my-func . args)
  (case-lambda
   (() "zero args")
   ((x) (+ x x))
   ((x y) (+ x y))
   ((x y . rest)
    (reduce + 0 (cons x (cons y rest)))
   ) ;
  ) ;case-lambda
) ;define

(check ((my-func)) => "zero args")
(check ((my-func) 2) => 4)
(check ((my-func) 3 4) => 7)
(check ((my-func) 1 2 3 4) => 10)

(check-report)
(if (check-failed?) (exit -1))
