(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; integer->char
;; 将整数码点转换为对应的字符。
;;
;; 语法
;; ----
;; (integer->char n)
;;
;; 参数
;; ----
;; n : integer?
;; 整数值，必须是有效的码点值，通常范围在0到255之间。
;; 返回值
;; ------
;; char?
;; 对应的字符
;;
;; 说明
;; ----
;; 1. 将整数转换为对应的字符
;; 4. 与char->integer互逆操作
;;
;;
;; 错误处理
;; --------
;; out-of-range
;; 当码点超出有效范围时抛出错误。
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

;; integer->char 基本测试
(check (integer->char 65) => #\A)
(check (integer->char 97) => #\a)
(check (integer->char 48) => #\0)
(check (integer->char 57) => #\9)
(check (integer->char 10) => #\newline)
(check (integer->char 32) => #\space)
(check (integer->char 9) => #\tab)

;; 大写和小写字符
(check (integer->char 65) => #\A)
(check (integer->char 90) => #\Z)
(check (integer->char 97) => #\a)
(check (integer->char 122) => #\z)

;; 数字字符
(check (integer->char 48) => #\0)
(check (integer->char 49) => #\1)
(check (integer->char 57) => #\9)

;; 特殊字符测试
(check (integer->char 33) => #\!)
(check (integer->char 64) => #\@)
(check (integer->char 35) => #\#)

;; 边界测试
(check (integer->char 0) => #\null)
(check (integer->char 126) => #\~)

;; 反向验证
(check (integer->char (char->integer #\A)) => #\A)
(check (integer->char (char->integer #\a)) => #\a)
(check (integer->char (char->integer #\0)) => #\0)
(check (char->integer (integer->char 65)) => 65)
(check (char->integer (integer->char 97)) => 97)

;; 错误处理测试
(check-catch 'out-of-range (integer->char -1))
(check-catch 'out-of-range (integer->char 256))
(check-catch 'wrong-type-arg (integer->char 65.0))
(check-catch 'wrong-number-of-args (integer->char))
(check-catch 'wrong-number-of-args (integer->char 65 66))  

(check-report)
