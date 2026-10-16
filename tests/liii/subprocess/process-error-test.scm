(import (liii check)
  (liii subprocess process-error)
  (liii subprocess)
) ;import

;; 1. 基本构造与谓词判断
(let ((err (make-process-error 1 "file not found\n" "" "cat /not/exist")))
  (check (process-error? err) => #t)
  (check (process-error? "not-an-error") => #f)
  (check (process-error? 123) => #f)
  (check (process-error? '()) => #f)

  ;; 2. 字段访问器验证
  (check (process-error-code err) => 1)
  (check (process-error-stderr err) => "file not found\n")
  (check (process-error-stdout err) => "")
  (check (process-error-command err) => "cat /not/exist")

  ;; 3. 便捷函数 process-error-message：优先使用 stderr
  (check (process-error-message err) => "file not found\n")
) ;let

;; 4. 便捷函数 process-error-message：stderr 为空时回退到 stdout
(let ((err (make-process-error 2 "" "compilation error\n" '("gcc" "foo.c"))))
  (check (process-error? err) => #t)
  (check (process-error-code err) => 2)
  (check (process-error-stderr err) => "")
  (check (process-error-stdout err) => "compilation error\n")
  (check (process-error-command err) => '("gcc" "foo.c"))
  (check (process-error-message err) => "compilation error\n")
) ;let

;; 5. 便捷函数 process-error-message：stderr 和 stdout 均为空时，生成带有退出码的信息
(let ((err (make-process-error 127 "" "" "unknown-command")))
  (check (process-error-code err) => 127)
  (check (process-error-message err) => "Command failed with exit code 127")
) ;let

;; 6. 异常类型检查
(check-catch 'type-error (process-error-message "invalid"))
(check-catch 'type-error (process-error-message 42))

(check-report)
