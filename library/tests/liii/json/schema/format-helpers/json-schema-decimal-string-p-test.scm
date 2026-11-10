;; json-schema-decimal-string?
;; 判断字符串是否只包含十进制数字。
;;
;; 语法
;; ----
;; (json-schema-decimal-string? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是非空十进制数字串返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 不接受正负号、小数点与指数部分。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check-true (json-schema-decimal-string? "0"))
(check-true (json-schema-decimal-string? "123"))
(check-true (json-schema-decimal-string? "00042"))

(check-false (json-schema-decimal-string? ""))
(check-false (json-schema-decimal-string? "12a"))
(check-false (json-schema-decimal-string? "-1"))
(check-false (json-schema-decimal-string? "1.5"))
(check-false (json-schema-decimal-string? "1e3"))

(check-report)
