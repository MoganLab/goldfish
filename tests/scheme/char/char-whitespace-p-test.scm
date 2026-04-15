(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-whitespace?
;; 判断字符是否为空白字符。
;;
;; 语法
;; ----
;; (char-whitespace? char) → boolean?
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
;; 控制字符测试
(check (char-whitespace? #\return) => #t)
(check (char-whitespace? #\backspace) => #f)
;; 非空白字符测试
(check (char-whitespace? #\a) => #f)
(check (char-whitespace? #\A) => #f)
(check (char-whitespace? #\0) => #f)
(check (char-whitespace? #\9) => #f)
(check (char-whitespace? #\!) => #f)
(check (char-whitespace? #\@) => #f)
;; 特殊边界测试
(check (char-whitespace? #\0) => #f)
(check (char-whitespace? #\a) => #f)
;; 符号
(check (char-whitespace? #\.) => #f)
(check (char-whitespace? #\,) => #f)
(check (char-whitespace? #\;) => #f)
;; 错误处理测试
(check-catch 'type-error (char-whitespace? 1))
(check-catch 'type-error (char-whitespace? " "))
(check-catch 'wrong-number-of-args (char-whitespace?))
(check-catch 'wrong-number-of-args
  (char-whitespace? #\space #\a)
) ;check-catch
(check-report)