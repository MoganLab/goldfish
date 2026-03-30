(import (liii check)
        (scheme char)
) ;import

(check-set-mode! 'report-failed)

;; char-numeric?
;; 判断字符是否为数字。
;;
;; 语法
;; ----
;; (char-numeric? char) → boolean?
;;
;; 参数
;; ----
;; char : character
;; 要判断的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符是数字则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于数字字符 #\0 到 #\9，返回 #t
;; - 对于非数字字符，返回 #f
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常

;; 数字范围测试
(check (char-numeric? #\0) => #t)
(check (char-numeric? #\1) => #t)
(check (char-numeric? #\2) => #t)
(check (char-numeric? #\3) => #t)
(check (char-numeric? #\4) => #t)
(check (char-numeric? #\5) => #t)
(check (char-numeric? #\6) => #t)
(check (char-numeric? #\7) => #t)
(check (char-numeric? #\8) => #t)
(check (char-numeric? #\9) => #t)

;; 非数字字符测试
(check (char-numeric? #\a) => #f)
(check (char-numeric? #\A) => #f)
(check (char-numeric? #\z) => #f)
(check (char-numeric? #\Z) => #f)
(check (char-numeric? #\!) => #f)
(check (char-numeric? #\@) => #f)
(check (char-numeric? #\#) => #f)

;; 特殊字符测试
(check (char-numeric? #\space) => #f)
(check (char-numeric? #\newline) => #f)
(check (char-numeric? #\tab) => #f)
(check (char-numeric? #\.) => #f)
(check (char-numeric? #\-) => #f)

;; 字母与数字边界测试
(check (char-numeric? #\/) => #f)
(check (char-numeric? #\:) => #f)

;; 错误处理测试
(check-catch 'type-error (char-numeric? 1))
(check-catch 'type-error (char-numeric? "1"))
(check-catch 'wrong-number-of-args (char-numeric?))
(check-catch 'wrong-number-of-args (char-numeric? #\1 #\2))

(check-report)
