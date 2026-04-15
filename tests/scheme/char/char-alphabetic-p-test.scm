(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-alphabetic?
;; 判断字符是否为字母。
;;
;; 语法
;; ----
;; (char-alphabetic? char) → boolean?
;;
;; 参数
;; ----
;; char : character
;; 要判断的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符是字母则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于字母字符（A-Z, a-z），返回 #t
;; - 对于非字母字符，返回 #f
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常
;; 小写字母测试
(check (char-alphabetic? #\a) => #t)
(check (char-alphabetic? #\b) => #t)
(check (char-alphabetic? #\z) => #t)
;; 大写字母测试
(check (char-alphabetic? #\A) => #t)
(check (char-alphabetic? #\B) => #t)
(check (char-alphabetic? #\Z) => #t)
;; 非字母字符测试
(check (char-alphabetic? #\0) => #f)
(check (char-alphabetic? #\1) => #f)
(check (char-alphabetic? #\9) => #f)
(check (char-alphabetic? #\!) => #f)
(check (char-alphabetic? #\@) => #f)
(check (char-alphabetic? #\#) => #f)
;; 特殊字符测试
(check (char-alphabetic? #\space) => #f)
(check (char-alphabetic? #\newline)
  =>
  #f
) ;check
(check (char-alphabetic? #\tab) => #f)
(check (char-alphabetic? #\return)
  =>
  #f
) ;check
;; 边界字符测试
(check (char-alphabetic? #\[) => #f)
(check (char-alphabetic? #\\) => #f)
(check (char-alphabetic? #\`) => #f)
(check (char-alphabetic? #\{) => #f)
;; 错误处理测试
(check-catch 'type-error
  (char-alphabetic? 1)
) ;check-catch
(check-catch 'type-error
  (char-alphabetic? "a")
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-alphabetic?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-alphabetic? #\a #\b)
) ;check-catch
(check-report)