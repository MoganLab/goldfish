(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let ((b1 (bag 1 1 2))
      (b2 (bag 1 1 2 2)))
  (check-true (bag>? b2 b1)))

(check-report)
