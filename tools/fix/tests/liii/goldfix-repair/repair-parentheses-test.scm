(import (liii check)
  (liii goldfix)
  (liii goldfix-record)
) ;import

(check-set-mode! 'report-failed)

;; repair-parentheses
;; 返回修正后的 source 和 repair-report。

(call-with-values (lambda ()
                    (repair-parentheses "(define x 1")
                  ) ;lambda
  (lambda (repaired report)
    (check repaired => "(define x 1)")
    (check (repair-report-ok? report) => #t)
    (check (length (repair-report-edits report))
      =>
      1
    ) ;check
    (check (fix-edit-kind (car (repair-report-edits report))
           ) ;fix-edit-kind
      =>
      'insert
    ) ;check
    (check (fix-edit-reason (car (repair-report-edits report))
           ) ;fix-edit-reason
      =>
      "eof"
    ) ;check
  ) ;lambda
) ;call-with-values

(call-with-values (lambda ()
                    (repair-parentheses "(define x 1))")
                  ) ;lambda
  (lambda (repaired report)
    (check repaired => "(define x 1)")
    (check (repair-report-ok? report) => #t)
    (check (length (repair-report-edits report))
      =>
      1
    ) ;check
    (check (fix-edit-kind (car (repair-report-edits report))
           ) ;fix-edit-kind
      =>
      'delete
    ) ;check
    (check (fix-edit-reason (car (repair-report-edits report))
           ) ;fix-edit-reason
      =>
      "extra-close"
    ) ;check
  ) ;lambda
) ;call-with-values

(check-report)
