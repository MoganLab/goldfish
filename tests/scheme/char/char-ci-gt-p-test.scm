(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-ci>?
;; 按大小写不敏感的方式比较字符是否大于。
;;
;; 语法
;; ----
;; (char-ci>? char1 char2 char3 ...) → boolean?
;;
;; 参数
;; ----
;; char1, char2, char3, ... : character
;; 要比较的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符序列在大小写不敏感的情况下严格降序，则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #f（相等）
;; - 对于不同的字符，按大小写不敏感的方式比较顺序
;; - 支持多个参数，只有当所有字符在大小写不敏感的情况下严格降序时才返回 #t
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
(check (char-ci>? #\B #\a) => #t)
(check (char-ci>? #\b #\A) => #t)
(check (char-ci>? #\a #\A) => #f)
(check (char-ci>? #\A #\a) => #f)
(check (char-ci>? #\a #\z) => #f)
(check (char-ci>? #\Z #\a) => #t)
;; 大小写一致测试
(check (char-ci>? #\b #\a) => #t)
(check (char-ci>? #\B #\A) => #t)
(check (char-ci>? #\a #\B) => #f)
(check (char-ci>? #\z #\A) => #t)
;; 相等字符测试（不严格降序）
(check (char-ci>? #\a #\A) => #f)
(check (char-ci>? #\A #\A) => #f)
(check (char-ci>? #\a #\a) => #f)
;; 特殊字符测试
(check (char-ci>? #\newline #\space)
  =>
  #f
) ;check
(check (char-ci>? #\space #\tab) => #t)
(check (char-ci>? #\@ #\!) => #t)
(check (char-ci>? #\! #\!) => #f)
;; 多参数不敏感降序测试
(check (char-ci>? #\D #\c #\B #\a)
  =>
  #t
) ;check
(check (char-ci>? #\d #\C #\b #\A)
  =>
  #t
) ;check
(check (char-ci>? #\z #\z #\a) => #f)
(check (char-ci>? #\b #\a #\C) => #f)
;; 数字字符测试（大小写不影响）
(check (char-ci>? #\9 #\0) => #t)
(check (char-ci>? #\0 #\9) => #f)
(check (char-ci>? #\z #\0) => #t)
(check (char-ci>? #\a #\0) => #t)
;; 边界测试
(check (char-ci>? #\z #\a) => #t)
(check (char-ci>? #\Z #\A) => #t)
(check (char-ci>? #\a #\z) => #f)
(check (char-ci>? #\A #\Z) => #f)
;; 非字母字符测试
(check (char-ci>? #\! #\0) => #f)
(check (char-ci>? #\~ #\0) => #t)
(check (char-ci>? #\~ #\space) => #t)
;; 错误处理测试
(check-catch 'type-error
  (char-ci>? 1 #\A)
) ;check-catch
(check-catch 'type-error
  (char-ci>? #\A 'symbol)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-ci>?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-ci>? #\A)
) ;check-catch
(check-report)
