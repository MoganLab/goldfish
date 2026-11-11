;; read-error?
;; 检查对象是否为读取错误。
;;
;;
;; 语法
;; ----
;; (read-error? obj)
;;
;; 参数
;; ----
;; obj : 任意对象
;; 要检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果对象是读取错误，返回 #t；否则返回 #f。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 测试错误对象（pair/list 格式）
(check (read-error? '(read-error)) => #t)
(check (read-error? '(read-error "message")) => #t)
(check (read-error? '(other-error)) => #f)

;; 测试标准 error 产生的对象（自动推断为 read-error）
(check (read-error? (guard (ex (else ex)) (error "read error in input"))) => #t)
(check (read-error? (guard (ex (else ex)) (error "unexpected EOF"))) => #t)
(check (read-error? (guard (ex (else ex)) (error "cannot open file: missing.scm"))) => #f)
(check (read-error? (guard (ex (else ex)) (error "general error"))) => #f)

;; 测试非错误对象安全返回 #f
(check (read-error? "error") => #f)
(check (read-error? 123) => #f)
(check (read-error? #t) => #f)
(check (read-error? #f) => #f)
(check (read-error? 'read-error) => #f)
(check (read-error? '()) => #f)

;; 测试与 file-error? 的区别
(check (read-error? '(read-error)) => #t)
(check (file-error? '(read-error)) => #f)
(check (read-error? '(io-error)) => #f)
(check (file-error? '(io-error)) => #t)

(check-report)
