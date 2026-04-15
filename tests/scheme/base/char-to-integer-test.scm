(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; char->integer
;; 将字符转换为其对应的码点值。
;;
;; 语法
;; ----
;; (char->integer char)
;;
;; 参数
;; ----
;; char : char?
;; 字符。
;;
;; 返回值
;; ------
;; integer?
;; 字符对应的码点值
;;
;; 说明
;; ----
;; 将字符转换为对应的整数值
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是字符时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。
;; char->integer 基本测试
(check (char->integer #\0) => 48)
(check (char->integer #\9) => 57)
;; 字符边界测试
(check (char->integer #\tab) => 9)
(check (char->integer #\newline) => 10)
(check (char->integer #\return) => 13)
(check (char->integer #\backspace) => 8)
;; 特殊字符测试
(check (char->integer #\!) => 33)
(check (char->integer #\@) => 64)
(check (char->integer #\#) => 35)
(check (char->integer #\$) => 36)
(check (char->integer #\%) => 37)
;; 扩展字符测试
(check (char->integer #\~) => 126)
(check (char->integer #\_) => 95)
;; 数字边界测试
(check (char->integer #\A) => 65)
(check (char->integer #\B) => 66)
(check (char->integer #\Z) => 90)
(check (char->integer #\a) => 97)
(check (char->integer #\z) => 122)
;; 错误处理测试
(check-catch 'wrong-type-arg (char->integer 65))
(check-catch 'wrong-type-arg (char->integer "A"))
(check-catch 'wrong-number-of-args (char->integer))
(check-catch 'wrong-number-of-args (char->integer #\A #\B))
(check-report)