;; signature-arity
;; 获取函数签名中 -> 之前的形参个数。
;;
;; 语法
;; ----
;; (signature-arity sig)
;;
;; 参数
;; ----
;; sig : list   待解析的签名列表
;;
;; 返回值
;; ------
;; integer 或 #f
;; 参数个数；签名语法不合法时返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 正确计算形参个数
(check (signature-arity '(-> any?)) => 0)
(check (signature-arity '(string? -> boolean?)) => 1)
(check (signature-arity '(any? integer? boolean? -> any?)) => 3)

;; 2. 非法签名返回 #f
(check (signature-arity '(string?)) => #f)

(check-report)
