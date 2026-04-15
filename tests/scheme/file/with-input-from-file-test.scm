(import (liii check) (scheme file))
(check-set-mode! 'report-failed)
(define test-file
  "tests/scheme/file/test-with-input.txt"
) ;define
;; 创建测试文件
(with-output-to-file test-file
  (lambda () (display "input from file"))
) ;with-output-to-file
;; 测试 with-input-from-file
(check (with-input-from-file test-file
         (lambda ()
           (read-string 100 (current-input-port))
         ) ;lambda
       ) ;with-input-from-file
  =>
  "input from file"
) ;check
;; 测试多行读取
(with-output-to-file test-file
  (lambda ()
    (display "first\n")
    (display "second")
  ) ;lambda
) ;with-output-to-file
(check (with-input-from-file test-file
         (lambda ()
           (list (read-line (current-input-port))
             (read-line (current-input-port))
           ) ;list
         ) ;lambda
       ) ;with-input-from-file
  =>
  '("first" "second")
) ;check
;; 测试中文文件名
(define chinese-file
  "tests/scheme/file/中文输入重定向.txt"
) ;define
(with-output-to-file chinese-file
  (lambda ()
    (display "中文输入内容")
  ) ;lambda
) ;with-output-to-file
(check (with-input-from-file chinese-file
         (lambda ()
           (read-string 100 (current-input-port))
         ) ;lambda
       ) ;with-input-from-file
  =>
  "中文输入内容"
) ;check
(delete-file chinese-file)
;; 清理
(delete-file test-file)
(check-report)
