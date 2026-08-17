(import (liii check))

(check-set-mode! 'report-failed)

(define x 1)
(define y x)
(define x 2)
(check y => 1)
(check x => 2)

(check-report)
