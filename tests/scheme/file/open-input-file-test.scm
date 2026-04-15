(import (liii check) (scheme file))
(check-set-mode! 'report-failed)
(define test-file "tests/scheme/file/test-open-input.txt")
;; 创建测试文件
(with-output-to-file test-file
  (lambda () (display "line1\n") (display "line2"))
) ;with-output-to-file
;; 测试 open-input-file
(let ((port (open-input-file test-file)))
  (check-true (input-port? port))
  (check (read-line port) => "line1")
  (check (read-line port) => "line2")
  (close-port port)
) ;let
;; 测试中文文件名
(define chinese-file "tests/scheme/file/中文输入.txt")
(with-output-to-file chinese-file
  (lambda () (display "第一行\n") (display "第二行"))
) ;with-output-to-file
(let ((port (open-input-file chinese-file)))
  (check-true (input-port? port))
  (check (read-line port) => "第一行")
  (check (read-line port) => "第二行")
  (close-port port)
) ;let
(delete-file chinese-file)
;; 清理
(delete-file test-file)
(check-report)