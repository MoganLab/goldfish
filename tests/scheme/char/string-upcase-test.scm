(import (liii check)
        (scheme char)
) ;import

(check-set-mode! 'report-failed)

;; string-upcase
;; 将字符串转换为大写形式。
;;
;; 语法
;; ----
;; (string-upcase str) → string?
;;
;; 参数
;; ----
;; str : string
;; 要转换的字符串
;;
;; 返回值
;; ------
;; string?
;; 转换后的大写字符串
;;
;; 注意
;; ----
;; - 小写字母被转换为大写
;; - 大写字母保持不变
;; - 非字母字符保持不变
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符串类型，否则会抛出异常

;; 基本功能测试
(check (string-upcase "hello") => "HELLO")
(check (string-upcase "HELLO") => "HELLO")
(check (string-upcase "Hello") => "HELLO")
(check (string-upcase "hElLo") => "HELLO")

;; 空字符串测试
(check (string-upcase "") => "")

;; 数字测试
(check (string-upcase "123") => "123")
(check (string-upcase "abc123") => "ABC123")
(check (string-upcase "123abc") => "123ABC")

;; 特殊字符测试
(check (string-upcase "hello world") => "HELLO WORLD")
(check (string-upcase "hello!") => "HELLO!")
(check (string-upcase "hello@world") => "HELLO@WORLD")
(check (string-upcase "hello_world") => "HELLO_WORLD")
(check (string-upcase "hello-world") => "HELLO-WORLD")

;; 混合测试
(check (string-upcase "AbCdEfG123") => "ABCDEFG123")
(check (string-upcase "Hello World!") => "HELLO WORLD!")

;; 错误处理测试
(check-catch 'type-error (string-upcase 123))
(check-catch 'type-error (string-upcase 'hello))
(check-catch 'type-error (string-upcase #\a))
(check-catch 'wrong-number-of-args (string-upcase))
(check-catch 'wrong-number-of-args (string-upcase "hello" "world"))

(check-report)
