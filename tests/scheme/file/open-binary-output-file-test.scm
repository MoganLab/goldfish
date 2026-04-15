(import (liii check) (scheme file))
(check-set-mode! 'report-failed)
(define test-file
  "tests/scheme/file/test-binary-output.bin"
) ;define
;; 测试 open-binary-output-file
(let ((port (open-binary-output-file test-file)))
  (check-true (output-port? port))
  (check-true (binary-port? port))
  (display "binary output" port)
  (close-port port)
) ;let
;; 验证内容
(let ((port (open-binary-input-file test-file)))
  (check (read-string 100 port) => "binary output")
  (close-port port)
) ;let
;; 测试中文文件名
(define chinese-file
  "tests/scheme/file/中文二进制输出.bin"
) ;define
(let ((port (open-binary-output-file chinese-file)))
  (check-true (output-port? port))
  (check-true (binary-port? port))
  (display "中文输出" port)
  (close-port port)
) ;let
(let ((port (open-binary-input-file chinese-file)))
  (check (read-string 100 port) => "中文输出")
  (close-port port)
) ;let
(delete-file chinese-file)
;; 清理
(delete-file test-file)
(check-report)