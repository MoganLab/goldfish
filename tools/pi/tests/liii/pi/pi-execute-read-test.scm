(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-execute-read
;; 执行 read 工具读取指定文件内容。
(define test-file
  (if (file-exists? "PI_PLAN.md")
    "PI_PLAN.md"
    "../../PI_PLAN.md"
  ) ;if
) ;define

(let ((content (pi-execute-read `(("path" . ,test-file) ("limit" . 2)))))
  (check-true (string? content)))

(check-report)
