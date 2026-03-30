(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

(let ((b (bag 1 2 2)))
  (bag-adjoin! b 2 3)
  (check (bag-size b) => 5))
(check-catch 'type-error (bag-adjoin! "not a bag" 1))

(check-report)
