(import (liii check) (liii pi) (scheme file))

(check-set-mode! 'report-failed)

;; pi-execute-write
;; 执行 write 工具将内容写入指定文件。
(let* ((target "/tmp/test-pi-write.txt")
       (_ (if (file-exists? target) (delete-file target) #t))
       (res (pi-execute-write `(("path" . ,target) ("content" . "hello write")))))
  (check-true (string? res))
  (check (file-exists? target) => #t)
  (delete-file target))

(check-report)
