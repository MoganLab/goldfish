;; json-schema-ascii-letter?
;; 判断字符是否为 ASCII 字母。
;;
;; 语法
;; ----
;; (json-schema-ascii-letter? char)
;;
;; 参数
;; ----
;; char : char
;; 待检查的字符。
;;
;; 返回值
;; ----
;; boolean
;; 是 ASCII 字母返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 只覆盖 U+0061 到 U+007A 与 U+0041 到 U+005A，不包含其他 Unicode 字母。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check-true (json-schema-ascii-letter? #\a))
(check-true (json-schema-ascii-letter? #\z))
(check-true (json-schema-ascii-letter? #\A))
(check-true (json-schema-ascii-letter? #\Z))

(check-false (json-schema-ascii-letter? #\0))
(check-false (json-schema-ascii-letter? #\9))
(check-false (json-schema-ascii-letter? #\_))
(check-false (json-schema-ascii-letter? #\space))

(check-report)
