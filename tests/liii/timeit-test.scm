

(import (liii check)
  (liii timeit)
  (liii time)
  (liii base)
) ;import


(check-set-mode! 'report-failed)





(let ((result (timeit (lambda () (+ 1 2))
                (lambda () #t)
                1000
              ) ;timeit
      ) ;result
     ) ;
  (check (number? result) => #t)
  (check (>= result 0) => #t)
) ;let


(let ((counter 0))
  (timeit (lambda () (set! counter (+ counter 1)))
    (lambda () (set! counter 0))
    100
  ) ;timeit
  (check (= counter 100) => #t)
) ;let


(let ((result1 (timeit (lambda () (* 2 3))
                 (lambda () #t)
                 100
               ) ;timeit
      ) ;result1
      (result2 (timeit (lambda () (* 2 3))
                 (lambda () #t)
                 1000
               ) ;timeit
      ) ;result2
     ) ;
  (check (number? result1) => #t)
  (check (number? result2) => #t)
  (check (>= result2 result1) => #t)
) ;let


(let ((result (timeit (lambda () (display ""))
                (lambda () #t)
                10
              ) ;timeit
      ) ;result
     ) ;
  (check (number? result) => #t)
  (check (>= result 0) => #t)
) ;let


(let ((lst '()))
  (timeit (lambda () (set! lst (cons 'x lst)))
    (lambda ()
      (set! lst (make-list 100 'a))
    ) ;lambda
    50
  ) ;timeit
  (check (= (length lst) 150) => #t)
) ;let


(check-catch 'type-error
  (timeit (lambda () #t)
    (lambda () #t)
    'invalid
  ) ;timeit
) ;check-catch


(check-catch 'type-error
  (timeit 'not-a-lambda
    (lambda () #t)
    100
  ) ;timeit
) ;check-catch


(check-catch 'type-error
  (timeit (lambda () #t)
    'not-a-lambda
    100
  ) ;timeit
) ;check-catch


(let ((result (timeit (lambda () (sleep 0.1))
                (lambda () #t)
                1
              ) ;timeit
      ) ;result
     ) ;
  (check (number? result) => #t)
  (check (>= result 0.09) => #t)
) ;let


(let ((result (timeit (lambda () (sleep 0.01))
                (lambda () #t)
                5
              ) ;timeit
      ) ;result
     ) ;
  (check (number? result) => #t)
  (check (>= result 0.04) => #t)
) ;let


(let ((result (timeit (lambda () (sleep 0.001))
                (lambda () #t)
                10
              ) ;timeit
      ) ;result
     ) ;
  (check (number? result) => #t)
  (check (>= result 0.005) => #t)
) ;let


(check-report)
