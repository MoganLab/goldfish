(import (liii check) (scheme file))
(check-set-mode! 'report-failed)
(define test-file
  "tests/scheme/file/test-with-output.txt"
) ;define
;; 清理可能存在的旧文件
(when (file-exists? test-file)
  (delete-file test-file)
) ;when
;; 测试 with-output-to-file
(with-output-to-file test-file
  (lambda () (display "output to file"))
) ;with-output-to-file
;; 验证文件内容
(check (call-with-input-file test-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "output to file"
) ;check
;; 测试覆盖写入
(with-output-to-file test-file
  (lambda () (display "new output"))
) ;with-output-to-file
(check (call-with-input-file test-file
         (lambda (port) (read-string 100 port))
       ) ;call-with-input-file
  =>
  "new output"
) ;check
;; 测试中文文件名
(define chinese-file
  "tests/scheme/file/中文输出重定向.txt"
) ;define
(with-output-to-file chinese-file
  (lambda ()
    (display "中文输出内容")
  ) ;lambda
) ;with-output-to-file
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
