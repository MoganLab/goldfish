(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let ((b (bag 1 2 2 3)))
  (define b2 (bag-delete b 2 3))
  (check (bag-size b) => 4)
  (check (bag-size b2) => 2)
  (check (bag-count (lambda (x) (= x 2)) b2) => 1))

(check-report)
