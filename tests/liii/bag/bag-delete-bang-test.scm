(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let ((b (bag 1 2 2 3)))
  (bag-delete! b 2 3)
  (check (bag-size b) => 2)
  (check (bag-count (lambda (x) (= x 2)) b) => 1))

(check-report)
