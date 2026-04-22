(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string<?
;; 按字典序比较字符串是否严格递增。
;;
;; 语法
;; ----
;; (string<? string1 string2 ...)
;;
;; 参数
;; ----
;; string1, string2, ... : string?
;; 至少两个字符串参数。
;;
;; 返回值
;; ------
;; boolean?
;; 如果每个字符串都严格小于后面的字符串，返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 基于字符的 Unicode 码点进行字典序比较
;; 2. 区分大小写
;; 3. 至少需要两个参数
(check (string<? "a" "b") => #t)
(check (string<? "b" "a") => #f)
(check (string<? "a" "a") => #f)
(check (string<? "abc" "abd") => #t)
(check (string<? "abd" "abc") => #f)
(check (string<? "a" "ab") => #t)
(check (string<? "ab" "a") => #f)
(check (string<? "A" "a") => #t)
(check (string<? "" "a") => #t)
(check (string<? "123" "124") => #t)
(check (string<? "a" "b" "c") => #t)
(check (string<? "a" "b" "b") => #f)
(check-catch 'wrong-number-of-args
  (string<?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string<? "a")
) ;check-catch
(check-catch 'wrong-type-arg
  (string<? 'a "b")
) ;check-catch
(check-catch 'wrong-type-arg
  (string<? "a" 1)
) ;check-catch

(check-report)
