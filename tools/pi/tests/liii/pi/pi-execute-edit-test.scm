(import (liii check) (liii pi) (scheme file))

(check-set-mode! 'report-failed)

;; pi-execute-edit
;; 执行 edit 工具在文件中替换指定代码块。
(let* ((target "/tmp/test-pi-edit.txt")
       (_ (call-with-output-file target (lambda (p) (display "hello old text" p))))
       (res (pi-execute-edit `(("path" . ,target)
                               ("edits" . ((("oldText" . "old") ("newText" . "new"))))))))
  (check-true (string? res))
  (delete-file target))

(check-report)
