(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; char<=?
;; 按字典序比较字符的大小，判断字符是否按非严格升序排列。
;;
;; 语法
;; ----
;; (char<=? char1 char2 char3 ...)
;;
;; 参数
;; ----
;; char1, char2, char3, ... : char?
;; 要比较的字符，至少需要两个。
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符按非严格升序排列（即每个字符都小于或等于下一个字符）则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 至少需要两个参数
;; 2. 所有参数必须都是字符
;; 3. 按字符的Unicode码点值进行比较
;; 4. 当字符按非严格升序排列时返回 #t，否则返回 #f
;; 5. 区分大小写，大写字符码点值小于小写字符
;; 6. 允许字符相等的情况
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是字符时抛出错误。
;; wrong-number-of-args
;; 当参数数量少于2个时抛出错误。
;; char<=? 基本测试
(check (char<=? #\A #\B) => #t)
(check (char<=? #\a #\b) => #t)
(check (char<=? #\A #\A) => #t)
(check (char<=? #\B #\A) => #f)
(check (char<=? #\0 #\9) => #t)
(check (char<=? #\9 #\0) => #f)
;; 相等字符测试
(check (char<=? #\A #\A) => #t)
(check (char<=? #\a #\a #\a) => #t)
(check (char<=? #\0 #\0) => #t)
;; 特殊字符测试
(check (char<=? #\space #\newline)
  =>
  #f
) ;check
(check (char<=? #\tab #\tab) => #t)
(check (char<=? #\newline #\space)
  =>
  #t
) ;check
;; 多参数非严格升序测试
(check (char<=? #\A #\B #\C) => #t)
(check (char<=? #\A #\A #\B) => #t)
(check (char<=? #\a #\b #\c) => #t)
(check (char<=? #\0 #\0 #\1 #\1 #\2)
  =>
  #t
) ;check
;; 多参数非升序测试
(check (char<=? #\A #\B #\A) => #f)
(check (char<=? #\b #\a) => #f)
(check (char<=? #\3 #\2 #\1) => #f)
;; 混合大小写测试
(check (char<=? #\A #\A #\a) => #t)
(check (char<=? #\Z #\a) => #t)
(check (char<=? #\b #\a #\Z) => #f)
;; 边界测试
(check (char<=? #\0 #\1 #\9) => #t)
(check (char<=? #\A #\Z) => #t)
(check (char<=? #\! #\~ #\~) => #t)
;; 数字字符测试
(check (char<=? #\1 #\2) => #t)
(check (char<=? #\5 #\5) => #t)
(check (char<=? #\9 #\8) => #f)
;; 错误处理测试
(check-catch 'wrong-type-arg
  (char<=? 1 #\A)
) ;check-catch
(check-catch 'wrong-type-arg
  (char<=? #\A 'symbol)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char<=?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char<=? #\A)
) ;check-catch
(check-report)
