(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-format-size
;; 格式化字节大小为人类可读字符串。
(check (pi-format-size 500) => "500B")
(check (pi-format-size 1024) => "1.0KB")
(check (pi-format-size 1536) => "1.5KB")

(check-report)
