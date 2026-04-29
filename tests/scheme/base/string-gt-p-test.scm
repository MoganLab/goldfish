(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string>?
;; 按字典序比较字符串是否严格递减。
;;
;; 语法
;; ----
;; (string>? string1 string2 ...)
;;
;; 参数
;; ----
;; string1, string2, ... : string?
;; 至少两个字符串参数。
;;
;; 返回值
;; ------
;; boolean?
;; 如果每个字符串都严格大于后面的字符串，返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 基于字符的 Unicode 码点进行字典序比较
;; 2. 区分大小写
;; 3. 至少需要两个参数
(check (string>? "b" "a") => #t)
(check (string>? "a" "b") => #f)
(check (string>? "a" "a") => #f)
(check (string>? "abd" "abc") => #t)
(check (string>? "abc" "abd") => #f)
(check (string>? "ab" "a") => #t)
(check (string>? "a" "ab") => #f)
(check (string>? "a" "A") => #t)
(check (string>? "a" "") => #t)
(check (string>? "124" "123") => #t)
(check (string>? "c" "b" "a") => #t)
(check (string>? "c" "b" "b") => #f)
(check-catch 'wrong-number-of-args (string>?))
(check-catch 'wrong-number-of-args (string>? "a"))
(check-catch 'wrong-type-arg (string>? 'a "b"))
(check-catch 'wrong-type-arg (string>? "a" 1))

(check-report)
