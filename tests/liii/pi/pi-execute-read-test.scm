(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-execute-read
;; 执行 read 工具读取指定文件内容。
(let ((content (pi-execute-read '(("path" . "PI_PLAN.md") ("limit" . 2)))))
  (check-true (string? content)))

(check-report)
