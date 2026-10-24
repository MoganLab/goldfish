(import (liii check) (liii pi find))

(check-set-mode! 'report-failed)

;; pi-execute-find
;; 执行 find 工具按 pattern 查找文件。
(let ((out (pi-execute-find '(("pattern" . "sample.txt")))))
  (check-true (string? out)))

(check-report)
