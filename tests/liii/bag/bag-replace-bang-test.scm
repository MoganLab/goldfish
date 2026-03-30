(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let* ((s1 "hello")
       (s2 (string-copy s1))
       (b (bag s1)))
  (bag-replace! b s2)
  (check-true (eq? (car (bag->list b)) s2)))

(check-report)
