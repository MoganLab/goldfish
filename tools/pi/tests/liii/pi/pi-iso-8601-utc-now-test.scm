(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-iso-8601-utc-now
;; 获取当前 UTC 时间的 ISO 8601 格式字符串。
(let ((now (pi-iso-8601-utc-now)))
  (check-true (string? now))
  (check (string-ref now 4) => #\-)
  (check (string-ref now 7) => #\-))

(check-report)
