(import (liii check) (scheme file))
(check-set-mode! 'report-failed)
(define test-file
  "tests/scheme/file/test-call-with-input-file.txt"
) ;define
;; 创建测试文件
(with-output-to-file test-file
  (lambda () (display "hello world"))
) ;with-output-to-file
;; 测试 call-with-input-file 基本功能
(check (call-with-input-file test-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "hello world"
) ;check
;; 测试读取多行
(with-output-to-file test-file
  (lambda () (display "line1\n") (display "line2"))
) ;with-output-to-file
(check (call-with-input-file test-file
         (lambda (port) (list (read-line port) (read-line port)))
       ) ;call-with-input-file
  =>
  '("line1" "line2")
) ;check
;; 测试中文文件名
(define chinese-file "tests/scheme/file/中文测试.txt")
(with-output-to-file chinese-file
  (lambda () (display "中文内容"))
) ;with-output-to-file
(check (call-with-input-file chinese-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "中文内容"
) ;check
(delete-file chinese-file)
;; 清理
(delete-file test-file)
(check-report)