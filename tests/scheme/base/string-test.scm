(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string
;; 从给定字符构造字符串。
;;
;; 语法
;; ----
;; (string char ...)
;;
;; 参数
;; ----
;; char ... : char?
;; 任意数量的字符。
;;
;; 返回值
;; ------
;; string?
;; 由给定字符组成的新字符串。
;;
;; 说明
;; ----
;; 1. 无参数时返回空字符串
;; 2. 参数顺序决定结果字符串中的字符顺序
(check (string) => "")
(check (string #\a) => "a")
(check (string #\a #\b #\c) => "abc")
(check (string #\1 #\2 #\3) => "123")
(check (string-length (string #\x #\y))
  =>
  2
) ;check
(check-catch 'wrong-type-arg
  (string 'a)
) ;check-catch
(check-catch 'wrong-type-arg (string 1))
(check-catch 'wrong-type-arg
  (string "a")
) ;check-catch
;; UTF-8 字符测试
(check (string=? (string #\中 #\文) "中文") => #t)
(check (string-length (string #\中 #\文)) => 6)
(check (string=? (string #\中) "中") => #t)
(check (string-length (string #\中)) => 3)
(check-report)
