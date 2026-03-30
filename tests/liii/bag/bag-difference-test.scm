(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let* ((b1 (bag 'a 'a 'b))
       (b2 (bag 'a 'b 'b 'c))
       (d (bag-difference b1 b2)))
  (check (bag-count (lambda (x) (eq? x 'a)) d) => 1)
  (check (bag-count (lambda (x) (eq? x 'b)) d) => 0))

(check-report)
