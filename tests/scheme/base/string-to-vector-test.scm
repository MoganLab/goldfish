(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string->vector
;; 将字符串转换为字符向量。
;;
;; 语法
;; ----
;; (string->vector string)
;;
;; 参数
;; ----
;; string : string?
;; 待转换的字符串。
;;
;; 返回值
;; ------
;; vector?
;; 由字符串中字符按顺序组成的新向量。
;;
;; 说明
;; ----
;; 1. 空字符串返回空向量
;; 2. 返回新向量，不修改原字符串
(check (string->vector "") => #())
(check (string->vector "a") => #(#\a))
(check (string->vector "abc") => #(#\a #\b #\c))
(check (string->vector "12") => #(#\1 #\2))
(check (vector-length (string->vector "xyz")) => 3)
(check (vector-ref (string->vector "abc") 0) => #\a)
(check-catch 'wrong-type-arg (string->vector))
(check (string->vector '()) => #())
(check (string->vector #(1 2)) => #(1 2))

(check-report)
