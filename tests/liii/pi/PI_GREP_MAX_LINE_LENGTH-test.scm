(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; PI_GREP_MAX_LINE_LENGTH
;; grep 单行输出最大字符截断长度 (500)。
(check PI_GREP_MAX_LINE_LENGTH => 500)

(check-report)
