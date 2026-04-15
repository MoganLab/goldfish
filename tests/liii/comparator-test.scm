(import (liii comparator)
  (liii check)
  (liii base)
) ;import

(check-set-mode! 'report-failed)

(let ((default-comp (make-default-comparator))
     ) ;
  (check-false (<? default-comp #t #t))
  (check-false (<? default-comp #f #f))
  (check-true (<? default-comp #f #t))
  (check-false (<? default-comp #t #f))
  (check-true (<? default-comp
                (cons #f #f)
                (cons #t #t)
              ) ;<?
  ) ;check-true
  (check-true (<? default-comp (list 1 2) (list 2 3))
  ) ;check-true
  (check-true (<? default-comp (list 1 2) (list 1 3))
  ) ;check-true
  (check-true (<? default-comp
                (vector "a" "b")
                (vector "b" "c")
              ) ;<?
  ) ;check-true

  (check-false (<? default-comp 1 1))
  (check-true (<? default-comp 0.0+1.0i 0.0+2.0i)
  ) ;check-true
  (check-true (<? default-comp 1.0+2.0i 2.0+2.0i)
  ) ;check-true

  (check (comparator-hash default-comp
           (list 1 2)
         ) ;comparator-hash
    =>
    42
  ) ;check
) ;let

(check-report)
