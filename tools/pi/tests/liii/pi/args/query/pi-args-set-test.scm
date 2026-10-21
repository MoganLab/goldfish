(import (liii check)
  (liii pi args query)
) ;import

(check-set-mode! 'report-failed)

(let* ((args '((key1 . "val1")))
       (updated (pi-args-set args 'key2 "val2"))
       (overridden (pi-args-set updated 'key1 "new1")))
  (check (pi-args-ref updated 'key1) => "val1")
  (check (pi-args-ref updated 'key2) => "val2")
  (check (pi-args-ref overridden 'key1) => "new1")
  (check (pi-args-ref overridden 'key2) => "val2")
) ;let*

(check-report)
