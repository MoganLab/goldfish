;; json-schema-ascii-digit?
;; 判断字符是否为 ASCII 数字。
;;
;; 语法
;; ----
;; (json-schema-ascii-digit? char)
;;
;; 参数
;; ----
;; char : char
;; 待检查的字符。
;;
;; 返回值
;; ----
;; boolean
;; 是 ASCII 数字返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 只覆盖 U+0030 到 U+0039，不包含其他 Unicode 数字。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check-true (json-schema-ascii-digit? #\0))
(check-true (json-schema-ascii-digit? #\5))
(check-true (json-schema-ascii-digit? #\9))

(check-false (json-schema-ascii-digit? #\a))
(check-false (json-schema-ascii-digit? #\A))
(check-false (json-schema-ascii-digit? #\-))
(check-false (json-schema-ascii-digit? #\space))

(check-report)
