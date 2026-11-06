;; uri-reference?
;; 判断字符串是否为 URI 引用。
;;
;; 语法
;; ----
;; (uri-reference? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的 URI 引用返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; URI 引用既可以是绝对 URI，也可以是相对引用，
;; 还可以带 fragment 与 query 部分。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-uri))

(check-set-mode! 'report-failed)

;; 绝对 URI 同时也是 URI 引用。

(check-true (uri-reference? "http://example.com"))
(check-true (uri-reference? "https://example.com/a?b=c#d"))

;; 相对引用。

(check-true (uri-reference? "/relative"))
(check-true (uri-reference? "relative/path"))
(check-true (uri-reference? ""))
(check-true (uri-reference? "#fragment"))
(check-true (uri-reference? "?query=1"))

;; 非法字符。

(check-false (uri-reference? "http://example.com/a b"))

(check-report)
