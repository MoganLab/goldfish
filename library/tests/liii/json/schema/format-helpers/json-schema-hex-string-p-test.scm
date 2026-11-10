;; json-schema-hex-string?
;; 判断字符串是否为十六进制数字串。
;;
;; 语法
;; ----
;; (json-schema-hex-string? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是非空十六进制数字串返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 长度上限为 4 个十六进制数字，用于标识符与短哈希片段。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check-true (json-schema-hex-string? "0"))
(check-true (json-schema-hex-string? "dead"))
(check-true (json-schema-hex-string? "DEAD"))
(check-true (json-schema-hex-string? "00ff"))

(check-false (json-schema-hex-string? ""))
(check-false (json-schema-hex-string? "deadbeef"))
(check-false (json-schema-hex-string? "xyz"))
(check-false (json-schema-hex-string? "0x1"))

(check-report)
