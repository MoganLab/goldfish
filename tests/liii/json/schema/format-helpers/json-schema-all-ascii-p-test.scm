;; json-schema-all-ascii?
;; 判断字符串是否只包含 ASCII 字符。
;;
;; 语法
;; ----
;; (json-schema-all-ascii? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 只包含 ASCII 字符返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 判定依据是码点是否大于 127，空串返回 #t。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check-true (json-schema-all-ascii? ""))
(check-true (json-schema-all-ascii? "abc"))
(check-true (json-schema-all-ascii? "user@example.com"))
(check-true (json-schema-all-ascii? "0123456789"))

(check-false (json-schema-all-ascii? "用户"))
(check-false (json-schema-all-ascii? "é"))

(check-report)
