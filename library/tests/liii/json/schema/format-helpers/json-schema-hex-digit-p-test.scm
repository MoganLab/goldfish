;; json-schema-hex-digit?
;; 判断字符是否为十六进制数字。
;;
;; 语法
;; ----
;; (json-schema-hex-digit? char)
;;
;; 参数
;; ----
;; char : char
;; 待检查的字符。
;;
;; 返回值
;; ----
;; boolean
;; 是十六进制数字返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 覆盖 U+0030 到 U+0039、U+0041 到 U+0046 与 U+0061 到 U+0066。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check-true (json-schema-hex-digit? #\0))
(check-true (json-schema-hex-digit? #\9))
(check-true (json-schema-hex-digit? #\a))
(check-true (json-schema-hex-digit? #\f))
(check-true (json-schema-hex-digit? #\A))
(check-true (json-schema-hex-digit? #\F))

(check-false (json-schema-hex-digit? #\g))
(check-false (json-schema-hex-digit? #\G))
(check-false (json-schema-hex-digit? #\-))

(check-report)
