;; absolute-uri?
;; 判断字符串是否为绝对 URI。
;;
;; 语法
;; ----
;; (absolute-uri? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 含 scheme 的绝对 URI 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 判定依据是字符串中存在冒号，用于区分绝对引用与相对引用。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema pointer))

(check-set-mode! 'report-failed)

(check-true (absolute-uri? "https://json-schema.org/draft/2020-12/schema"))
(check-true (absolute-uri? "urn:example:resource"))
(check-true (absolute-uri? "a:b"))

;; 相对引用没有冒号。

(check-false (absolute-uri? "meta/core"))
(check-false (absolute-uri? "../schema"))
(check-false (absolute-uri? ""))
(check-false (absolute-uri? "/absolute/path"))

(check-report)
