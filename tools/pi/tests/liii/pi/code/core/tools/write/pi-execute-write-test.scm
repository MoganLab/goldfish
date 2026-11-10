;; pi-execute-write
;; 写入文件内容
;;
;; 语法
;; ----
;; (pi-execute-write args)
;;
;; 参数
;; ----
;; args : list?，工具参数
;;
;; 返回值
;; ------
;; record?，工具结果

(import (liii check)
  (liii string)
  (liii path)
  (liii pi code core tools)
  (scheme file)
) ;import

(check-set-mode! 'report-failed)

;; pi-execute-write
;; 执行 write 工具将内容写入指定文件。
(let* ((target (path->string (path-join (path-temp-dir) "test-pi-write.txt")))
       (_ (if (file-exists? target) (delete-file target) #t))
       (res
         (pi-execute-write `(("path" . ,target) ("content" . "hello write")))
       ) ;res
      ) ;
  (check-true (string? res))
  (check (file-exists? target) => #t)
  (delete-file target)
) ;let*

;; 写入、读取、覆盖与参数错误
(let ((test-write-file "tests/liii/pi-write-fixture/sub/test.txt"))
  (let ((msg (pi-execute-write (list (cons "path" test-write-file) (cons "content" "hello write tool\nline 2"))
             ) ;pi-execute-write
        ) ;msg
       ) ;
    (check msg => (string-append "Successfully wrote to " test-write-file))
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "hello write tool\nline 2")
    ) ;let
  ) ;let
  (let ((msg (pi-execute-write (list (cons "path" test-write-file) (cons "content" "overwritten content"))
             ) ;pi-execute-write
        ) ;msg
       ) ;
    (check msg => (string-append "Successfully wrote to " test-write-file))
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "overwritten content")
    ) ;let
  ) ;let
  (delete-file test-write-file)
  (delete-file "tests/liii/pi-write-fixture/sub")
  (delete-file "tests/liii/pi-write-fixture")
) ;let

(check-catch 'error (pi-execute-write '(("path" . "foo.txt"))))
(check-catch 'error (pi-execute-write '(("content" . "hello"))))
(check-catch 'error
  (pi-execute-write '(("path" . "foo.txt") ("content" . "bar")) :timed-out? #t)
) ;check-catch

(check-report)
