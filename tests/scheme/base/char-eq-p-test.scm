(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; char=?
;; 比较两个或多个字符是否相等。
;;
;; 语法
;; ----
;; (char=? char1 char2 . more-chars)
;;
;; 参数
;; ----
;; char1, char2, ... : char?
;; 字符值。
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有给定的字符都相等，则返回 #t (真)，否则返回 #f (假)。
;;
;; 说明
;; ----
;; 1. 至少需要两个参数
;; 2. 所有参数必须都是字符
;; 3. 当所有字符相等时返回 #t，否则返回 #f
;; 4. 支持比较两个或多个字符
;; 5. 区分大小写
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是字符时抛出错误。
;; wrong-number-of-args
;; 当参数数量少于2个时抛出错误。

;; char=? 基本测试
(check (char=? #\A #\A) => #t)
(check (char=? #\a #\a) => #t)
(check (char=? #\A #\a) => #f)
(check (char=? #\a #\A) => #f)
(check (char=? #\0 #\0) => #t)
(check (char=? #\9 #\9) => #t)
(check (char=? #\0 #\9) => #f)

;; 特殊字符测试
(check (char=? #\space #\space) => #t)
(check (char=? #\newline #\newline) => #t)
(check (char=? #\tab #\tab) => #t)
(check (char=? #\space #\newline) => #f)

;; 多参数测试
(check (char=? #\A #\A #\A) => #t)
(check (char=? #\a #\a #\a) => #t)
(check (char=? #\A #\A #\a) => #f)
(check (char=? #\a #\b #\c) => #f)

;; 边界测试
(check (char=? #\0 #\0 #\0 #\0 #\0) => #t)
(check (char=? #\A #\A #\A #\A #\a) => #f)
(check (char=? #\z #\z #\z) => #t)
(check (char=? #\! #\! #\!) => #t)

;; 数字字符测试
(check (char=? #\1 #\1) => #t)
(check (char=? #\1 #\! ) => #f)

;; 大小写混合测试
(check (char=? #\a #\b #\c #\d) => #f)
(check (char=? #\A #\B #\C) => #f)

;; 错误处理测试
(check-catch 'wrong-type-arg (char=? 1 #\A))
(check-catch 'wrong-type-arg (char=? #\A 'symbol))
(check-catch 'wrong-type-arg (char=? 123 #\a))
(check-catch 'wrong-number-of-args (char=?))
(check-catch 'wrong-number-of-args (char=? #\A))

(check-report)
