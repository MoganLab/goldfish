(import (liii check) (liii pi ls))

(check-set-mode! 'report-failed)

;; pi-execute-ls
;; 执行 ls 工具列出目录文件清单。
(let ((out (pi-execute-ls '(("path" . ".")))))
  (check-true (string? out)))

(check-report)
