(import (liii check) (liii string) (liii pi code core compaction serialize))

(check-set-mode! 'report-failed)

(let ((long (make-string 30 #\x)))
  (check (pi-truncate-for-summary "short" 10) => "short")
  (check (pi-truncate-for-summary long 10)
    =>
    (string-append (make-string 10 #\x) "\n\n[... 20 more characters truncated]")
  ) ;check
) ;let

(check-report)
