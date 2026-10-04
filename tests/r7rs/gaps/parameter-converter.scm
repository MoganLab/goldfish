;; Known 4.2.6 gap: restoring a parameter applies its converter a second time.
(import (scheme base) (liii check))
(check-set-mode! 'report-failed)
(let ((p (make-parameter 1 (lambda (x) (+ x 1)))))
  (check (p) => 2)
  (check (parameterize ((p 10)) (p)) => 11)
  (check (p) => 2))
(check-report)
