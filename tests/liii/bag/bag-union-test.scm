(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let* ((b1 (bag 'a 'a 'b))
       (b2 (bag 'a 'b 'b 'c))
       (u (bag-union b1 b2)))
  (check (bag-count (lambda (x) (eq? x 'a)) u) => 2)
  (check (bag-count (lambda (x) (eq? x 'b)) u) => 2)
  (check (bag-count (lambda (x) (eq? x 'c)) u) => 1))

(check-report)
