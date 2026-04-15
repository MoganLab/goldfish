(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-ci<?
;; 按大小写不敏感的方式比较字符是否小于。
;;
;; 语法
;; ----
;; (char-ci<? char1 char2 char3 ...) → boolean?
;;
;; 参数
;; ----
;; char1, char2, char3, ... : character
;; 要比较的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符序列在大小写不敏感的情况下严格升序，则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #f（相等）
;; - 对于不同的字符，按大小写不敏感的方式比较顺序
;; - 支持多个参数，只有当所有字符在大小写不敏感的情况下严格升序时才返回 #t
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 所有参数必须是字符类型，否则会抛出异常
;;
;; wrong-number-of-args
;; 至少需要两个参数，否则会抛出异常
;; 基本功能测试
(check (char-ci<? #\a #\B) => #t)
(check (char-ci<? #\A #\b) => #t)
(check (char-ci<? #\A #\a) => #f)
(check (char-ci<? #\a #\A) => #f)
(check (char-ci<? #\Z #\a) => #f)
(check (char-ci<? #\z #\A) => #f)
;; 大小写一致测试
(check (char-ci<? #\a #\b) => #t)
(check (char-ci<? #\A #\B) => #t)
(check (char-ci<? #\B #\a) => #f)
(check (char-ci<? #\z #\A) => #f)
;; 相等字符测试
(check (char-ci<? #\a #\A) => #f)
(check (char-ci<? #\A #\A) => #f)
(check (char-ci<? #\a #\a) => #f)
;; 特殊字符测试
(check (char-ci<? #\space #\newline) => #f)
(check (char-ci<? #\tab #\space) => #t)
(check (char-ci<? #\! #\@) => #t)
(check (char-ci<? #\! #\!) => #f)
;; 多参数测试
(check (char-ci<? #\a #\B #\c #\D) => #t)
(check (char-ci<? #\A #\b #\C #\d) => #t)
(check (char-ci<? #\a #\A #\b #\B) => #f)
(check (char-ci<? #\a #\z #\A #\B) => #f)
;; 数字字符测试
(check (char-ci<? #\0 #\1) => #t)
(check (char-ci<? #\9 #\0) => #f)
(check (char-ci<? #\1 #\A) => #t)
(check (char-ci<? #\9 #\a) => #t)
;; 字母范围测试
(check (char-ci<? #\a #\z) => #t)
(check (char-ci<? #\A #\Z) => #t)
(check (char-ci<? #\z #\a) => #f)
(check (char-ci<? #\Z #\a) => #f)
;; 特殊字符边界测试
(check (char-ci<? #\0 #\!) => #f)
(check (char-ci<? #\space #\!) => #t)
(check (char-ci<? #\tab #\newline) => #t)
;; 错误处理测试
(check-catch 'type-error (char-ci<? 1 #\A))
(check-catch 'type-error (char-ci<? #\A 'symbol))
(check-catch 'wrong-number-of-args (char-ci<?))
(check-catch 'wrong-number-of-args (char-ci<? #\A))
(check-report)