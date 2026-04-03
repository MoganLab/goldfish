(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; read-error?
;; 检查对象是否为读取错误。
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
;; ------
;; boolean?
;; 如果对象是读取错误，返回 #t；否则返回 #f。

;; 测试错误对象（pair/list 格式）
(check (read-error? '(read-error)) => #t)
(check (read-error? '(read-error "message")) => #t)
(check (read-error? '(other-error)) => #f)

;; 测试非错误对象（使用 guard 来安全地测试）
(check (guard (ex (else #f)) (read-error? "error")) => #f)
(check (guard (ex (else #f)) (read-error? 123)) => #f)
(check (guard (ex (else #f)) (read-error? #t)) => #f)
(check (guard (ex (else #f)) (read-error? 'read-error)) => #f)

;; 测试与 file-error? 的区别
(check (read-error? '(read-error)) => #t)
(check (file-error? '(read-error)) => #f)
(check (read-error? '(io-error)) => #f)
(check (file-error? '(io-error)) => #t)

(check-report)
