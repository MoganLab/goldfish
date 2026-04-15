(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-upper-case?
;; 判断字符是否为大写字母字符。
;;
;; 语法
;; ----
;; (char-upper-case? char) → boolean?
;;
;; 参数
;; ----
;; char : character
;; 要判断的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符是大写字母则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于大写字母字符（A-Z），返回 #t
;; - 对于小写字母、数字、符号和空白字符，返回 #f
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常
;; 大写字母测试
(check (char-upper-case? #\A) => #t)
(check (char-upper-case? #\B) => #t)
(check (char-upper-case? #\Z) => #t)
;; 小写字母测试
(check (char-upper-case? #\a) => #f)
(check (char-upper-case? #\z) => #f)
(check (char-upper-case? #\b) => #f)
;; 特殊测试
(check (char-upper-case? #\@) => #f)
(check (char-upper-case? #\[) => #f)
(check (char-upper-case? #\`) => #f)
;; 非字母字符测试
(check (char-upper-case? #\0) => #f)
(check (char-upper-case? #\9) => #f)
(check (char-upper-case? #\!) => #f)
(check (char-upper-case? #\space) => #f)
(check (char-upper-case? #\newline)
  =>
  #f
) ;check
;; 混合测试
(check (char-upper-case? #\@) => #f)
(check (char-upper-case? #\_) => #f)
(check (char-upper-case? #\`) => #f)
;; 字母测试
(check (char-upper-case? #\M) => #t)
(check (char-upper-case? #\m) => #f)
;; 错误处理测试
(check-catch 'type-error
  (char-upper-case? 1)
) ;check-catch
(check-catch 'type-error
  (char-upper-case? "A")
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-upper-case?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-upper-case? #\A #\B)
) ;check-catch
(check-report)
