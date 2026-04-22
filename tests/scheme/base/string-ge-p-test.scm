(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string>=?
;; 按字典序比较字符串是否非递增。
;;
;; 语法
;; ----
;; (string>=? string1 string2 ...)
;;
;; 参数
;; ----
;; string1, string2, ... : string?
;; 至少两个字符串参数。
;;
;; 返回值
;; ------
;; boolean?
;; 如果每个字符串都大于或等于后面的字符串，返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 基于字符的 Unicode 码点进行字典序比较
;; 2. 区分大小写
;; 3. 允许相等关系
(check (string>=? "b" "a") => #t)
(check (string>=? "a" "b") => #f)
(check (string>=? "a" "a") => #t)
(check (string>=? "abd" "abc") => #t)
(check (string>=? "abc" "abd") => #f)
(check (string>=? "ab" "a") => #t)
(check (string>=? "a" "ab") => #f)
(check (string>=? "a" "A") => #t)
(check (string>=? "a" "") => #t)
(check (string>=? "124" "123") => #t)
(check (string>=? "c" "b" "a") => #t)
(check (string>=? "c" "b" "b") => #t)
(check (string>=? "c" "b" "c") => #f)
(check-catch 'wrong-number-of-args
  (string>=?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string>=? "a")
) ;check-catch
(check-catch 'wrong-type-arg
  (string>=? 'a "b")
) ;check-catch
(check-catch 'wrong-type-arg
  (string>=? "a" 1)
) ;check-catch

(check-report)
