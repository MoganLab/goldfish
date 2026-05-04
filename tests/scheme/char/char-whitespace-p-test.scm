(import (liii check) (scheme char))
(check-set-mode! 'report-failed)

;; char-whitespace?
;; 判断字符是否为空白字符。
;;
;; 语法
;; ----
;; (char-whitespace? char) -> boolean?
;;
;; 参数
;; ----
;; char : character
;; 要判断的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符是空白字符则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于空白字符（空格、换行符、制表符等），返回 #t
;; - 对于非空白字符，返回 #f
;; - 当前实现基于 Chez Scheme 10.3 的 char-whitespace? 行为
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常

;; 标准空白测试
(check (char-whitespace? #\space) => #t)
(check (char-whitespace? #\newline) => #t)
(check (char-whitespace? #\tab) => #t)
(check (char-whitespace? #\return) => #t)

;; Unicode 空白字符测试（基于 Chez Scheme 10.3）
(check (char-whitespace? #\x000B) => #t)
(check (char-whitespace? #\x000C) => #t)
(check (char-whitespace? #\x0085) => #t)
(check (char-whitespace? #\x00A0) => #t)
(check (char-whitespace? #\x1680) => #t)
(check (char-whitespace? #\x2000) => #t)
(check (char-whitespace? #\x2001) => #t)
(check (char-whitespace? #\x2002) => #t)
(check (char-whitespace? #\x2003) => #t)
(check (char-whitespace? #\x2004) => #t)
(check (char-whitespace? #\x2005) => #t)
(check (char-whitespace? #\x2006) => #t)
(check (char-whitespace? #\x2007) => #t)
(check (char-whitespace? #\x2008) => #t)
(check (char-whitespace? #\x2009) => #t)
(check (char-whitespace? #\x200A) => #t)
(check (char-whitespace? #\x2028) => #t)
(check (char-whitespace? #\x2029) => #t)
(check (char-whitespace? #\x202F) => #t)
(check (char-whitespace? #\x205F) => #t)
(check (char-whitespace? #\x3000) => #t)

;; 非空白字符测试
(check (char-whitespace? #\x0041) => #f)
(check (char-whitespace? #\x0061) => #f)
(check (char-whitespace? #\x0030) => #f)
(check (char-whitespace? #\x0021) => #f)
(check (char-whitespace? #\x4E2D) => #f)

;; 错误处理测试
(check-catch 'type-error (char-whitespace? 1))
(check-catch 'type-error (char-whitespace? " "))
(check-catch 'wrong-number-of-args (char-whitespace?))
(check-catch 'wrong-number-of-args (char-whitespace? #\space #\a))

(check-report)
