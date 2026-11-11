;; error-object?
;; 判断对象是否为错误对象。
;;
;;
;; 语法
;; ----
;; (error-object? obj)
;;
;; 参数
;; ----
;; obj : 任意类型
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果 obj 是错误对象则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 由 error 创建的对象满足 error-object? 谓词
;; 2. 非错误对象（如数字、字符串、符号、列表等）安全返回 #f

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(define err (guard (ex (else ex)) (error "an error occurred" 1 2 3)))

(check (error-object? err) => #t)
(check (error-object? (guard (ex (else ex)) (error "boom"))) => #t)
(check (error-object? "not an error") => #f)
(check (error-object? 123) => #f)
(check (error-object? 'error) => #f)
(check (error-object? '()) => #f)
(check (error-object? '(io-error)) => #f)
(check (error-object? #t) => #f)
(check (error-object? #f) => #f)

(check-report)
