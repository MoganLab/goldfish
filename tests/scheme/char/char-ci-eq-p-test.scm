(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-ci=?
;; 按大小写不敏感的方式比较字符是否相等。
;;
;; 语法
;; ----
;; (char-ci=? char1 char2 char3 ...) → boolean?
;;
;; 参数
;; ----
;; char1, char2, char3, ... : character
;; 要比较的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符在大小写不敏感的情况下都相等，则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #t
;; - 对于相同的字符（无论大小写），返回 #t
;; - 对于不同的字符，返回 #f
;; - 支持多个参数，只有当所有字符在大小写不敏感的情况下都相等时才返回 #t
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
(check (char-ci=? #\a #\A) => #t)
(check (char-ci=? #\A #\a) => #t)
(check (char-ci=? #\z #\Z) => #t)
(check (char-ci=? #\Z #\z) => #t)
(check (char-ci=? #\b #\B) => #t)
(check (char-ci=? #\B #\b) => #t)
;; 大小写一致测试
(check (char-ci=? #\a #\a) => #t)
(check (char-ci=? #\A #\A) => #t)
(check (char-ci=? #\1 #\1) => #t)
;; 不同大小写混合测试
(check (char-ci=? #\a #\b) => #f)
(check (char-ci=? #\a #\B) => #f)
(check (char-ci=? #\A #\b) => #f)
(check (char-ci=? #\A #\z) => #f)
(check (char-ci=? #\Z #\a) => #f)
;; 多参数测试
(check (char-ci=? #\a #\a #\A) => #t)
(check (char-ci=? #\A #\a #\a) => #t)
(check (char-ci=? #\z #\Z #\z #\Z)
  =>
  #t
) ;check
(check (char-ci=? #\a #\b #\A) => #f)
(check (char-ci=? #\A #\B #\a) => #f)
;; 数字字符测试（char-ci不影响数字）
(check (char-ci=? #\0 #\0) => #t)
(check (char-ci=? #\1 #\1) => #t)
(check (char-ci=? #\1 #\2) => #f)
;; 特殊字符测试
(check (char-ci=? #\space #\space)
  =>
  #t
) ;check
(check (char-ci=? #\newline #\newline)
  =>
  #t
) ;check
(check (char-ci=? #\! #\!) => #t)
(check (char-ci=? #\! #\@) => #f)
;; 大小写转换边界测试
(check (char-ci=? #\a #\A #\b #\B)
  =>
  #f
) ;check
(check (char-ci=? #\A #\a #\A) => #t)
(check (char-ci=? #\m #\M #\m) => #t)
(check (char-ci=? #\M #\m #\M) => #t)
;; 边界字符测试
(check (char-ci=? #\0 #\a) => #f)
(check (char-ci=? #\A #\z) => #f)
(check (char-ci=? #\Z #\a) => #f)
;; 错误处理测试
(check-catch 'type-error
  (char-ci=? 1 #\A)
) ;check-catch
(check-catch 'type-error
  (char-ci=? #\A 'symbol)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-ci=?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-ci=? #\A)
) ;check-catch
(check-report)
