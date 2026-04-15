(import (liii check)
        (scheme char)
) ;import

(check-set-mode! 'report-failed)

;; string-downcase
;; 将字符串转换为小写形式。
;;
;; 语法
;; ----
;; (string-downcase str) → string?
;;
;; 参数
;; ----
;; str : string
;; 要转换的字符串
;;
;; 返回值
;; ------
;; string?
;; 转换后的小写字符串
;;
;; 注意
;; ----
;; - 大写字母被转换为小写
;; - 小写字母保持不变
;; - 非字母字符保持不变
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符串类型，否则会抛出异常

;; 基本功能测试
(check (string-downcase "HELLO") => "hello")
(check (string-downcase "hello") => "hello")
(check (string-downcase "Hello") => "hello")
(check (string-downcase "hElLo") => "hello")

;; 空字符串测试
(check (string-downcase "") => "")

;; 数字测试
(check (string-downcase "123") => "123")
(check (string-downcase "ABC123") => "abc123")
(check (string-downcase "123ABC") => "123abc")

;; 特殊字符测试
(check (string-downcase "HELLO WORLD") => "hello world")
(check (string-downcase "HELLO!") => "hello!")
(check (string-downcase "HELLO@WORLD") => "hello@world")
(check (string-downcase "HELLO_WORLD") => "hello_world")
(check (string-downcase "HELLO-WORLD") => "hello-world")

;; 混合测试
(check (string-downcase "AbCdEfG123") => "abcdefg123")
(check (string-downcase "Hello World!") => "hello world!")

;; 错误处理测试
(check-catch 'type-error (string-downcase 123))
(check-catch 'type-error (string-downcase 'hello))
(check-catch 'type-error (string-downcase #\a))
(check-catch 'wrong-number-of-args (string-downcase))
(check-catch 'wrong-number-of-args (string-downcase "hello" "world"))

(check-report)
