;; signature-parts
;; 解析函数签名并将参数类型列表与返回类型拆分为二元表。
;;
;; 语法
;; ----
;; (signature-parts sig)
;;
;; 参数
;; ----
;; sig : list   待解析的签名列表（如 '(string? -> integer?)）
;;
;; 返回值
;; ------
;; list 或 #f
;; 成功解析返回 (params-list return-type)，语法错误返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 标准函数签名拆分
(check (signature-parts '(string? -> boolean?)) => '((string?) boolean?))
(check (signature-parts '(any? integer? -> any?)) => '((any? integer?) any?))

;; 2. 语法错误时返回 #f
(check (signature-parts '(string? boolean?)) => #f)
(check (signature-parts '(->)) => #f)

(check-report)
