(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-lower-case?
;; 判断字符是否为小写字母字符。
;;
;; 语法
;; ----
;; (char-lower-case? char) → boolean?
;;
;; 参数
;; ----
;; char : character
;; 要判断的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符是小写字母则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于小写字母字符（a-z），返回 #t
;; - 对于大写字母、数字、符号和空白字符，返回 #f
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常
;; 小写字母测试
(check (char-lower-case? #\a) => #t)
(check (char-lower-case? #\b) => #t)
(check (char-lower-case? #\z) => #t)
;; 大写字母测试
(check (char-lower-case? #\A) => #f)
(check (char-lower-case? #\B) => #f)
(check (char-lower-case? #\Z) => #f)
;; 特殊测试
(check (char-lower-case? #\`) => #f)
(check (char-lower-case? #\{) => #f)
;; 非字母字符测试
(check (char-lower-case? #\0) => #f)
(check (char-lower-case? #\9) => #f)
(check (char-lower-case? #\!) => #f)
(check (char-lower-case? #\space) => #f)
(check (char-lower-case? #\newline) => #f)
;; 混合测试
(check (char-lower-case? #\a) => #t)
(check (char-lower-case? #\z) => #t)
(check (char-lower-case? #\_) => #f)
(check (char-lower-case? #\`) => #f)
(check (char-lower-case? #\{) => #f)
;; 字母测试
(check (char-lower-case? #\m) => #t)
(check (char-lower-case? #\M) => #f)
;; 错误处理测试
(check-catch 'type-error (char-lower-case? 1))
(check-catch 'type-error (char-lower-case? "a"))
(check-catch 'wrong-number-of-args (char-lower-case?))
(check-catch 'wrong-number-of-args
  (char-lower-case? #\a #\b)
) ;check-catch
(check-report)