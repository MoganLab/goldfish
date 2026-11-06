;; email-address?
;; 判断字符串是否为合法的电子邮件地址。
;;
;; 语法
;; ----
;; (email-address? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的电子邮件地址返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 本地部分可以是点原子或带引号的字符串，域名可以是主机名或 IP 字面量，
;; 整个地址只能包含 ASCII 字符。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-email))

(check-set-mode! 'report-failed)

;; 常见的合法地址。

(check-true (email-address? "user@example.com"))
(check-true (email-address? "first.last@example.com"))
(check-true (email-address? "user+tag@example.com"))
(check-true (email-address? "user_name@example.com"))
(check-true (email-address? "user-name@sub.example.com"))
(check-true (email-address? "user@localhost"))
(check-true (email-address? "\"quoted local\"@example.com"))
(check-true (email-address? "user@[127.0.0.1]"))

;; 缺少 @ 或存在多个 @。

(check-false (email-address? "user"))
(check-false (email-address? "user@@example.com"))
(check-false (email-address? "@example.com"))
(check-false (email-address? "user@"))

;; 本地部分不合法。

(check-false (email-address? ".user@example.com"))
(check-false (email-address? "user.@example.com"))
(check-false (email-address? "us..er@example.com"))
(check-false (email-address? "user name@example.com"))

;; 域名不合法。

(check-false (email-address? "user@example..com"))
(check-false (email-address? "user@-example.com"))

;; 非 ASCII 字符不被接受。

(check-false (email-address? "用户@example.com"))

(check-report)
