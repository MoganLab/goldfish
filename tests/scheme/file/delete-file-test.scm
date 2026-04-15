(import (liii check) (scheme file))
(check-set-mode! 'report-failed)
(define test-file
  "tests/scheme/file/test-delete-file.txt"
) ;define
;; 创建测试文件
(with-output-to-file test-file
  (lambda () (display "test"))
) ;with-output-to-file
;; 验证文件存在
(check-true (file-exists? test-file))
;; 测试 delete-file
(check (delete-file test-file) => #t)
;; 验证文件已删除
(check-false (file-exists? test-file))
;; 测试删除不存在的文件应报错
(check-catch 'read-error
  (delete-file "tests/scheme/file/non-existent-file.txt"
  ) ;delete-file
) ;check-catch
;; 测试删除参数类型错误
(check-catch 'type-error
  (delete-file 123)
) ;check-catch
;; 测试删除中文文件名
(define chinese-file
  "tests/scheme/file/中文删除.txt"
) ;define
(with-output-to-file chinese-file
  (lambda () (display "test"))
) ;with-output-to-file
(check-true (file-exists? chinese-file))
(check (delete-file chinese-file) => #t)
(check-false (file-exists? chinese-file)
) ;check-false
(check-report)
