(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let ((b1 (bag 1 1 2))
      (b2 (bag 1 1 2 2))
      (b3 (bag 1 1 2)))
  (check-true (bag=? b1 b3))
  (check-false (bag=? b1 b2)))

(check-report)
