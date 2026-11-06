;; uri?
;; 判断字符串是否为绝对 URI。
;;
;; 语法
;; ----
;; (uri? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的绝对 URI 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 绝对 URI 必须带 scheme，形如 scheme:path；
;; 相对引用不带 scheme，因此不属于绝对 URI。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-uri))

(check-set-mode! 'report-failed)

;; 合法绝对 URI。

(check-true (uri? "http://example.com"))
(check-true (uri? "https://example.com/a?b=c#d"))
(check-true (uri? "urn:example:resource"))
(check-true (uri? "mailto:user@example.com"))
(check-true (uri? "a+b-c.d:e"))

;; 相对引用不带 scheme。

(check-false (uri? "/relative"))
(check-false (uri? "example.com"))
(check-false (uri? "relative/path"))

;; scheme 不合法。

(check-false (uri? "://x"))
(check-false (uri? "1http://example.com"))

(check-report)
