;; json-schema-ascii-alnum?
;; 判断字符是否为 ASCII 字母或数字。
;;
;; 语法
;; ----
;; (json-schema-ascii-alnum? char)
;;
;; 参数
;; ----
;; char : char
;; 待检查的字符。
;;
;; 返回值
;; ----
;; boolean
;; 是 ASCII 字母或数字返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 是 json-schema-ascii-digit? 与 json-schema-ascii-letter? 的并集。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check-true (json-schema-ascii-alnum? #\0))
(check-true (json-schema-ascii-alnum? #\9))
(check-true (json-schema-ascii-alnum? #\a))
(check-true (json-schema-ascii-alnum? #\z))
(check-true (json-schema-ascii-alnum? #\A))
(check-true (json-schema-ascii-alnum? #\Z))

(check-false (json-schema-ascii-alnum? #\-))
(check-false (json-schema-ascii-alnum? #\_))
(check-false (json-schema-ascii-alnum? #\space))

(check-report)
