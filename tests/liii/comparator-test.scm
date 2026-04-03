(import (liii comparator)
        (liii check)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

(let ((default-comp (make-default-comparator)))
  (check-false (<? default-comp #t #t))
  (check-false (<? default-comp #f #f))
  (check-true (<?  default-comp #f #t))
  (check-false (<? default-comp #t #f))
  (check-true (<? default-comp (cons #f #f) (cons #t #t)))
  (check-true (<? default-comp (list 1 2) (list 2 3)))
  (check-true (<? default-comp (list 1 2) (list 1 3)))
  (check-true (<? default-comp (vector "a" "b") (vector "b" "c")))

  (check-false (<? default-comp 1 1))
  (check-true (<? default-comp 0+1i 0+2i))
  (check-true (<? default-comp 1+2i 2+2i))

  (check (comparator-hash default-comp (list 1 2)) => 42)
) ;let

(check-report)

