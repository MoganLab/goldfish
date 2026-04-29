(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string->list
;; 将字符串转换为字符列表。
;;
;; 语法
;; ----
;; (string->list string)
;;
;; 参数
;; ----
;; string : string?
;; 待转换的字符串。
;;
;; 返回值
;; ------
;; list?
;; 字符串中字符按顺序组成的列表。
;;
;; 说明
;; ----
;; 1. 空字符串返回空列表
;; 2. 返回的列表中每个元素都是字符
(check (string->list "") => '())
(check (string->list "a") => '(#\a))
(check (string->list "abc") => '(#\a #\b #\c))
(check (string->list "123") => '(#\1 #\2 #\3))
(check (length (string->list "hello")) => 5)
(check (car (string->list "xyz")) => #\x)
(check (string->list "abcde" 1 4) => '(#\b #\c #\d))
(check-catch 'wrong-type-arg (string->list '()))
(check-catch 'wrong-type-arg (string->list '(#\a)))
(check-catch 'wrong-number-of-args (string->list))
(check-catch 'wrong-type-arg (string->list "a" "b"))

(check-report)
