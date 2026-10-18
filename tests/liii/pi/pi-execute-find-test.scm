(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-execute-find
;; 执行 find 工具按 pattern 查找文件。
(let ((out (pi-execute-find '(("pattern" . "PI_PLAN.md")))))
  (check-true (string? out)))

(check-report)
