(import (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; PI_DEFAULT_MAX_BYTES
;; 默认最大输出字节限制 (50KB = 51200)。
(check PI_DEFAULT_MAX_BYTES => 51200)

(check-report)
