;; idn-email-address?
;; 判断字符串是否为合法的国际化电子邮件地址。
;;
;; 语法
;; ----
;; (idn-email-address? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的国际化电子邮件地址返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 与 email-address? 相比，本地部分与域名允许非 ASCII 字符，
;; 但仍要求两者非空且不以点结尾。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-email))

(check-set-mode! 'report-failed)

;; ASCII 地址同样是合法的国际化地址。

(check-true (idn-email-address? "user@example.com"))
(check-true (idn-email-address? "first.last@example.com"))

;; 非 ASCII 的本地部分与域名。

(check-true (idn-email-address? "用户@example.com"))
(check-true (idn-email-address? "user@例子.测试"))

;; 缺少 @ 或存在多个 @。

(check-false (idn-email-address? "user"))
(check-false (idn-email-address? "user@@example.com"))
(check-false (idn-email-address? "@example.com"))

;; 本地部分或域名为空。

(check-false (idn-email-address? "@example.com"))
(check-false (idn-email-address? "user@"))

;; 以点结尾不被接受。

(check-false (idn-email-address? "user.@example.com"))
(check-false (idn-email-address? "user@example.com."))

(check-report)
