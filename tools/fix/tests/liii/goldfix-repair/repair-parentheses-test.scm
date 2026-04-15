(import (liii check)
        (liii goldfix)
        (liii goldfix-record))

(check-set-mode! 'report-failed)

;; repair-parentheses
;; 返回修正后的 source 和 repair-report。

(call-with-values
  (lambda () (repair-parentheses "(define x 1"))
  (lambda (repaired report)
    (check repaired => "(define x 1)")
    (check (repair-report-ok? report) => #t)
    (check (length (repair-report-edits report)) => 1)
    (check (fix-edit-kind (car (repair-report-edits report))) => 'insert)
    (check (fix-edit-reason (car (repair-report-edits report))) => "eof")))

(call-with-values
  (lambda () (repair-parentheses "(define x 1))"))
  (lambda (repaired report)
    (check repaired => "(define x 1)")
    (check (repair-report-ok? report) => #t)
    (check (length (repair-report-edits report)) => 1)
    (check (fix-edit-kind (car (repair-report-edits report))) => 'delete)
    (check (fix-edit-reason (car (repair-report-edits report))) => "extra-close")))

(check-report)
