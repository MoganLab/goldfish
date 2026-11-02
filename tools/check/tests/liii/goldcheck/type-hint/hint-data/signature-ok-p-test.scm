;; signature-ok?
;; 校验单条函数签名的完整语法与所有类型指示符是否合法。
;;
;; 语法
;; ----
;; (signature-ok? sig)
;;
;; 参数
;; ----
;; sig : list   待校验的签名列表
;;
;; 返回值
;; ------
;; boolean
;; 签名合法返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 合法签名验证
(check (signature-ok? '(string? -> boolean?)) => #t)
(check (signature-ok? '(string? (or #f integer?) -> any?)) => #t)

;; 2. 非法签名验证
(check (signature-ok? '(string? boolean?)) => #f)
(check (signature-ok? '(123 -> any?)) => #f)

(check-report)
