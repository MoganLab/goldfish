(import (liii check)
        (liii date-time))

(check-set-mode! 'report-failed)

;; 回归测试：仅经伞模块 (liii date-time) 导入时，
;; iso-8601->timestamp 内部依赖 date+clock-time->timestamp
;; （来自 internal/timestamp-construct），曾经因 timestamp-iso
;; 缺少该 import 而在这种加载路径下报 unbound variable。
(let ((ts (iso-8601->timestamp "2026-01-02T03:04:05Z")))
  (check-true (timestamp? ts))
  (check (timestamp->utc-posix-time ts) => 1767323045))

(check (timestamp->utc-posix-time (iso-8601->timestamp "2026-10-20T08:00:02.000Z"))
  => 1792483202)

(check-report)
