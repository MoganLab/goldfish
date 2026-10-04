(import (scheme base) (scheme eval) (scheme write) (liii check))
(check-set-mode! 'report-failed)
(include "../fixtures/semantic-audit.scm")
;; Isolated because the current native list? traversal does not detect cycles.
(audit-check 'data.list-cycle
  (lambda () (let ((x (list 1))) (set-cdr! x x) (list? x))) #f)
(check-report)
