(import (liii check) (liii string) (liii path) (liii pi code core tools) (scheme file))

(check-set-mode! 'report-failed)

;; pi-execute-write
;; 执行 write 工具将内容写入指定文件。
(let* ((target (path->string (path-join (path-temp-dir) "test-pi-write.txt")))
       (_ (if (file-exists? target) (delete-file target) #t))
       (res (pi-execute-write `(("path" . ,target) ("content" . "hello write")))))
  (check-true (string? res))
  (check (file-exists? target) => #t)
  (delete-file target))

;; 写入、读取、覆盖与参数错误
(let ((test-write-file "tests/liii/pi-write-fixture/sub/test.txt"))
  (let ((msg (pi-execute-write
               (list (cons "path" test-write-file)
                     (cons "content" "hello write tool\nline 2")))))
    (check msg => (string-append "Successfully wrote to " test-write-file))
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "hello write tool\nline 2")))
  (let ((msg (pi-execute-write
               (list (cons "path" test-write-file)
                     (cons "content" "overwritten content")))))
    (check msg => (string-append "Successfully wrote to " test-write-file))
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "overwritten content")))
  (delete-file test-write-file)
  (delete-file "tests/liii/pi-write-fixture/sub")
  (delete-file "tests/liii/pi-write-fixture"))

(check-catch 'error (pi-execute-write '(("path" . "foo.txt"))))
(check-catch 'error (pi-execute-write '(("content" . "hello"))))
(check-catch 'error
  (pi-execute-write '(("path" . "foo.txt") ("content" . "bar")) :timed-out? #t))

(check-report)
