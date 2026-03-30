(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let* ((b1 (bag 'a 'a 'b))
       (b2 (bag 'a 'b 'b 'c)))
  (bag-xor! b1 b2)
  (check (bag-count (lambda (x) (eq? x 'a)) b1) => 1)
  (check (bag-count (lambda (x) (eq? x 'b)) b1) => 1)
  (check (bag-count (lambda (x) (eq? x 'c)) b1) => 1))

(check-report)
