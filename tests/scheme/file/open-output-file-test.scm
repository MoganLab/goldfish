(import (liii check) (scheme file))
(check-set-mode! 'report-failed)
(define test-file
  "tests/scheme/file/test-open-output.txt"
) ;define
;; 测试 open-output-file
(let ((port (open-output-file test-file)))
  (check-true (output-port? port))
  (display "output content" port)
  (close-port port)
) ;let
;; 验证内容
(check (call-with-input-file test-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "output content"
) ;check
;; 测试覆盖写入
(let ((port (open-output-file test-file)))
  (display "new content" port)
  (close-port port)
) ;let
(check (call-with-input-file test-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "new content"
) ;check
;; 测试中文文件名
(define chinese-file
  "tests/scheme/file/中文输出文件.txt"
) ;define
(let ((port (open-output-file chinese-file)))
  (check-true (output-port? port))
  (display "中文输出内容" port)
  (close-port port)
) ;let
(check (call-with-input-file chinese-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "中文输出内容"
) ;check
(delete-file chinese-file)
;; 清理
(delete-file test-file)
(check-report)
