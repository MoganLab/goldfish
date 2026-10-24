(import (liii check) (liii pi tools))

(check-set-mode! 'report-failed)

;; PI_DEFAULT_MAX_LINES
;; 默认最大输出行数限制 (2000)。
(check PI_DEFAULT_MAX_LINES => 2000)

(check-report)
