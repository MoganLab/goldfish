(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let* ((b1 (bag 'a 'a 'b))
       (b2 (bag 'a 'b 'b 'c))
       (x (bag-xor b1 b2)))
  (check (bag-count (lambda (x) (eq? x 'a)) x) => 1)
  (check (bag-count (lambda (x) (eq? x 'b)) x) => 1)
  (check (bag-count (lambda (x) (eq? x 'c)) x) => 1))

(check-report)
