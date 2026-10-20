(import (liii check)
  (liii pi args query)
) ;import

(check-set-mode! 'report-failed)

(let ((args '((key1 . "val1") (key2 . 42))))
  (check (pi-args-ref args 'key1) => "val1")
  (check (pi-args-ref args 'key2) => 42)
  (check (pi-args-ref args 'not-found) => #f)
  (check (pi-args-ref args 'not-found "default") => "default")
) ;let

(check-report)
