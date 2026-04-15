(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; file-error?
;; 检查对象是否为文件错误。
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
;; ------
;; boolean?
;; 如果对象是文件错误，返回 #t；否则返回 #f。
;; 测试错误对象（pair/list 格式）
(check (file-error? '(io-error)) => #t)
(check (file-error? '(io-error "message")) => #t)
(check (file-error? '(read-error)) => #f)
;; 测试非错误对象（使用 guard 来安全地测试）
(check (guard (ex (else #f)) (file-error? "error")) => #f)
(check (guard (ex (else #f)) (file-error? 123)) => #f)
(check (guard (ex (else #f)) (file-error? #t)) => #f)
(check (guard (ex (else #f)) (file-error? 'io-error)) => #f)
;; 测试 read-error? 与 file-error? 的区别
(check (read-error? '(read-error)) => #t)
(check (file-error? '(read-error)) => #f)
(check-report)