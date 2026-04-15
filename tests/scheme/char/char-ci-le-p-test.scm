(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-ci<=?
;; 按大小写不敏感的方式比较字符是否小于等于。
;;
;; 语法
;; ----
;; (char-ci<=? char1 char2 char3 ...) → boolean?
;;
;; 参数
;; ----
;; char1, char2, char3, ... : character
;; 要比较的字符
;;
;; 返回值
;; ------
;; boolean?
;; 如果字符序列在大小写不敏感的情况下非严格升序，则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #t（相等）
;; - 对于不同的字符，按大小写不敏感的方式比较顺序
;; - 支持多个参数，只有当所有字符在大小写不敏感的情况下非严格升序时才返回 #t
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
(check (char-ci<=? #\a #\B) => #t)
(check (char-ci<=? #\A #\b) => #t)
(check (char-ci<=? #\A #\a) => #t)
(check (char-ci<=? #\a #\A) => #t)
(check (char-ci<=? #\Z #\a) => #f)
(check (char-ci<=? #\z #\a) => #f)
;; 大小写一致测试
(check (char-ci<=? #\a #\b) => #t)
(check (char-ci<=? #\A #\B) => #t)
(check (char-ci<=? #\B #\a) => #f)
(check (char-ci<=? #\z #\A) => #f)
;; 相等字符测试（非严格升序）
(check (char-ci<=? #\a #\A) => #t)
(check (char-ci<=? #\A #\a) => #t)
(check (char-ci<=? #\A #\A) => #t)
(check (char-ci<=? #\a #\a) => #t)
;; 多参数非严格升序测试
(check (char-ci<=? #\a #\B #\c #\D)
  =>
  #t
) ;check
(check (char-ci<=? #\A #\a #\b #\B)
  =>
  #t
) ;check
(check (char-ci<=? #\A #\A #\B #\b)
  =>
  #t
) ;check
(check (char-ci<=? #\a #\a #\a) => #t)
(check (char-ci<=? #\z #\a #\b) => #f)
;; 字母范围测试
(check (char-ci<=? #\a #\z) => #t)
(check (char-ci<=? #\A #\Z) => #t)
(check (char-ci<=? #\z #\z) => #t)
(check (char-ci<=? #\0 #\9) => #t)
;; 特殊字符测试
(check (char-ci<=? #\space #\newline)
  =>
  #f
) ;check
(check (char-ci<=? #\tab #\tab) => #t)
(check (char-ci<=? #\@ #\newline) => #f)
(check (char-ci<=? #\! #\") => #t)
(check (char-ci<=? #\! #\!) => #t)
(check (char-ci<=? #\0 #\A) => #t)
(check (char-ci<=? #\9 #\z) => #t)
(check (char-ci<=? #\A #\a #\Z) => #t)
(check (char-ci<=? #\Z #\a #\Z) => #f)
(check-catch 'type-error
  (char-ci<=? 1 #\A)
) ;check-catch
(check-catch 'type-error
  (char-ci<=? #\A 'symbol)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-ci<=?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char-ci<=? #\A)
) ;check-catch
(check-report)
