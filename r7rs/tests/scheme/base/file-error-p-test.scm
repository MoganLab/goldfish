;; file-error?
;; 检查对象是否为文件错误。
;;
;;
;; 语法
;; ----
;; (file-error? obj)
;;
;; 参数
;; ----
;; obj : 任意对象
;; 要检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果对象是文件错误，返回 #t；否则返回 #f。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 测试错误对象（pair/list 格式）
(check (file-error? '(io-error)) => #t)
(check (file-error? '(file-error)) => #t)
(check (file-error? '(io-error "message")) => #t)
(check (file-error? '(read-error)) => #f)

;; 测试真实文件操作与显式错误标签产生的错误对象
(check (file-error? (guard (ex (else ex)) (open-input-file "tests/scheme/base/nonexistent-file-12345.scm"))) => #t)
(check (error-object? (guard (ex (else ex)) (open-input-file "tests/scheme/base/nonexistent-file-12345.scm"))) => #t)
(check (file-error? (guard (ex (else ex)) (error 'file-error "file error occurred"))) => #t)
(check (file-error? (guard (ex (else ex)) (error 'io-error "io error occurred"))) => #t)

;; 测试标准 error 产生的通用错误对象（不应被判定为 file-error）
(check (file-error? (guard (ex (else ex)) (error "cannot open file: missing.scm"))) => #f)
(check (file-error? (guard (ex (else ex)) (error "no such file or directory"))) => #f)
(check (file-error? (guard (ex (else ex)) (error "file error occurred"))) => #f)
(check (file-error? (guard (ex (else ex)) (error "general error"))) => #f)

;; 测试非错误对象安全返回 #f
(check (file-error? "error") => #f)
(check (file-error? 123) => #f)
(check (file-error? #t) => #f)
(check (file-error? #f) => #f)
(check (file-error? 'io-error) => #f)
(check (file-error? '()) => #f)

;; 测试 read-error? 与 file-error? 的区别
(check (read-error? '(read-error)) => #t)
(check (file-error? '(read-error)) => #f)

(check-report)
