(import (liii check)
  (liii subprocess process-error)
) ;import

;; 测试 process-error 记录构造与访问器
(let ((err (make-process-error "echo hi" 1 "out" "err" "custom message")))
  (check (process-error? err) => #t)
  (check (process-error-command err) => "echo hi")
  (check (process-error-code err) => 1)
  (check (process-error-stdout err) => "out")
  (check (process-error-stderr err) => "err")
  (check (process-error-message err) => "custom message")
) ;let

;; 测试缺省 message 构造 (自动从 stderr 提取)
(let ((err (make-process-error "test-cmd" 2 "out" "error output\n")))
  (check (process-error? err) => #t)
  (check (process-error-command err) => "test-cmd")
  (check (process-error-code err) => 2)
  (check (process-error-stdout err) => "out")
  (check (process-error-stderr err) => "error output\n")
  (check (process-error-message err) => "error output\n")
) ;let

;; 测试缺省 message 构造 (stderr 为空，stdout 非空)
(let ((err (make-process-error "test-cmd" 2 "only stdout\n" "")))
  (check (process-error? err) => #t)
  (check (process-error-command err) => "test-cmd")
  (check (process-error-code err) => 2)
  (check (process-error-stdout err) => "only stdout\n")
  (check (process-error-stderr err) => "")
  (check (process-error-message err) => "only stdout\n")
) ;let

;; 测试缺省 message 构造 (stderr 与 stdout 均为空)
(let ((err (make-process-error "test-cmd" 127 "" "")))
  (check (process-error? err) => #t)
  (check (process-error-command err) => "test-cmd")
  (check (process-error-code err) => 127)
  (check (process-error-stdout err) => "")
  (check (process-error-stderr err) => "")
  (check (process-error-message err) => "Command 'test-cmd' exited with status 127")
) ;let

;; 测试列表形式 command
(let ((err (make-process-error '("cat" "nonexistent") 1 "" "No such file\n")))
  (check (process-error? err) => #t)
  (check (process-error-command err) => '("cat" "nonexistent"))
  (check (process-error-code err) => 1)
  (check (process-error-stdout err) => "")
  (check (process-error-stderr err) => "No such file\n")
  (check (process-error-message err) => "No such file\n")
) ;let

;; 测试 code 在首位的构造兼容签名
(let ((err (make-process-error 42 "some error" "some out" "my-cmd")))
  (check (process-error? err) => #t)
  (check (process-error-command err) => "my-cmd")
  (check (process-error-code err) => 42)
  (check (process-error-stdout err) => "some out")
  (check (process-error-stderr err) => "some error")
  (check (process-error-message err) => "some error")
) ;let

;; 谓词防御性测试
(check (process-error? 123) => #f)
(check (process-error? "error") => #f)
(check (process-error? '(1 . "err")) => #f)
(check (process-error? #f) => #f)

(check-report)
