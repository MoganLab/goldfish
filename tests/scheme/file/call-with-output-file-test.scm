(import (liii check) (scheme file))
(check-set-mode! 'report-failed)
(define test-file
  "tests/scheme/file/test-call-with-output-file.txt"
) ;define
;; 测试 call-with-output-file 基本功能
(call-with-output-file test-file
  (lambda (port)
    (display "test content" port)
  ) ;lambda
) ;call-with-output-file
;; 验证文件内容
(check (call-with-input-file test-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "test content"
) ;check
;; 测试覆盖写入
(call-with-output-file test-file
  (lambda (port)
    (display "new content" port)
  ) ;lambda
) ;call-with-output-file
(check (call-with-input-file test-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "new content"
) ;check
;; 测试中文文件名
(define chinese-file
  "tests/scheme/file/中文输出.txt"
) ;define
(call-with-output-file chinese-file
  (lambda (port)
    (display "中文写入内容" port)
  ) ;lambda
) ;call-with-output-file
(check (call-with-input-file chinese-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "中文写入内容"
) ;check
(delete-file chinese-file)
;; 清理
(delete-file test-file)
(check-report)