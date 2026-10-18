(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-execute-grep
;; 执行 grep 工具按正则表达式或字面量搜索文件内容。
(let ((out (pi-execute-grep '(("pattern" . "PI_PLAN") ("path" . "PI_PLAN.md")))))
  (check-true (string? out)))

(check-report)
