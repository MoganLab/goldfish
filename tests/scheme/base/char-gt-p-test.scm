(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; char>?
;; 按字典序比较字符的大小，判断字符是否按降序排列。
;;
;; 语法
;; ----
;; (char>? char1 char2 char3 ...)
;;
;; 参数
;; ----
;; char1, char2, char3, ... : char?
;; 要比较的字符，至少需要两个。
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符按降序排列（即每个字符都大于下一个字符）则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 至少需要两个参数
;; 2. 所有参数必须都是字符
;; 3. 按字符的Unicode码点值进行比较
;; 4. 当字符按严格降序排列时返回 #t，否则返回 #f
;; 5. 区分大小写，大写字符码点值小于小写字符（如 #\A < #\a）
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是字符时抛出错误。
;; wrong-number-of-args
;; 当参数数量少于2个时抛出错误。
;; char>? 基本测试
(check (char>? #\B #\A) => #t)
(check (char>? #\b #\a) => #t)
(check (char>? #\a #\A) => #t)
(check (char>? #\A #\a) => #f)
(check (char>? #\9 #\0) => #t)
(check (char>? #\0 #\9) => #f)
;; 相等字符测试
(check (char>? #\A #\A) => #f)
(check (char>? #\a #\a) => #f)
(check (char>? #\0 #\0) => #f)
;; 特殊字符测试
(check (char>? #\newline #\space) => #f)
(check (char>? #\space #\tab) => #t)
(check (char>? #\tab #\newline) => #f)
;; 多参数降序测试
(check (char>? #\C #\B #\A) => #t)
(check (char>? #\c #\b #\a) => #t)
(check (char>? #\4 #\3 #\2 #\1 #\0)
  =>
  #t
) ;check
(check (char>? #\% #\$ #\# #\! #\~)
  =>
  #f
) ;check
;; 多参数非降序测试
(check (char>? #\B #\A #\B) => #f)
(check (char>? #\a #\a #\b) => #f)
(check (char>? #\1 #\2 #\3) => #f)
;; 混合大小写测试
(check (char>? #\b #\a #\Z) => #t)
(check (char>? #\z #\a #\Z) => #t)
(check (char>? #\A #\Z #\a) => #f)
;; 边界测试
(check (char>? #\9 #\0) => #t)
(check (char>? #\Z #\A) => #t)
(check (char>? #\z #\a) => #t)
(check (char>? #\~ #\!) => #t)
;; 数字字符测试
(check (char>? #\2 #\1) => #t)
(check (char>? #\5 #\5) => #f)
(check (char>? #\8 #\9) => #f)
;; 错误处理测试
(check-catch 'wrong-type-arg
  (char>? 1 #\A)
) ;check-catch
(check-catch 'wrong-type-arg
  (char>? #\A 'symbol)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char>?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (char>? #\A)
) ;check-catch
(check-report)
