(import (liii check)
        (scheme file)
) ;import

(check-set-mode! 'report-failed)

(define test-file "tests/scheme/file/test-binary-input.bin")

;; 创建测试文件（二进制内容）
(with-output-to-file test-file
  (lambda ()
    (display "binary content")
    (newline)
  ) ;lambda
) ;with-output-to-file

;; 测试 open-binary-input-file
(let ((port (open-binary-input-file test-file)))
  (check-true (input-port? port))
  (check-true (binary-port? port))
  (check
    (read-string 100 port)
    => "binary content\n"
  ) ;check
  (close-port port)
) ;let

;; 测试中文文件名
(define chinese-file "tests/scheme/file/中文二进制输入.bin")
(with-output-to-file chinese-file
  (lambda () (display "中文二进制内容"))
) ;with-output-to-file
(let ((port (open-binary-input-file chinese-file)))
  (check-true (input-port? port))
  (check-true (binary-port? port))
  (check
    (read-string 100 port)
    => "中文二进制内容"
  ) ;check
  (close-port port)
) ;let
(delete-file chinese-file)

;; 清理
(delete-file test-file)

(check-report)
