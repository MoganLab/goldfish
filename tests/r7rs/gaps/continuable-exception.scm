;; Known 6.11 gap: the handler currently aborts instead of resuming raise.
(import (scheme base) (liii check))
(check-set-mode! 'report-failed)
(check (with-exception-handler (lambda (obj) obj)
         (lambda () (+ 1 (raise-continuable 3)))) => 4)
(check-report)
