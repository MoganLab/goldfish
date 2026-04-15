(import (liii check)
        (scheme file)
) ;import

(check-set-mode! 'report-failed)

(define test-file "tests/scheme/file/test-exists.txt")

;; 确保测试文件不存在
(when (file-exists? test-file)
  (delete-file test-file)
) ;when

;; 测试不存在的文件
(check-false (file-exists? test-file))

;; 创建文件
(with-output-to-file test-file
  (lambda () (display "test"))
) ;with-output-to-file

;; 测试存在的文件
(check-true (file-exists? test-file))

;; 清理
(delete-file test-file)

;; 再次测试不存在的文件
(check-false (file-exists? test-file))

;; 测试目录也存在
(check-true (file-exists? "tests/scheme/file"))

;; 测试参数类型错误
(check-catch
  'type-error
  (file-exists? 123)
) ;check-catch

;; 测试中文文件名
(define chinese-file "tests/scheme/file/中文存在.txt")
(when (file-exists? chinese-file)
  (delete-file chinese-file)
) ;when
(check-false (file-exists? chinese-file))
(with-output-to-file chinese-file
  (lambda () (display "test"))
) ;with-output-to-file
(check-true (file-exists? chinese-file))
(delete-file chinese-file)
(check-false (file-exists? chinese-file))

(check-report)
