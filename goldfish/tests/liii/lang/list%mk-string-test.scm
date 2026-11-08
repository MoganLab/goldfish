;; list%mk-string
;; 将链表元素拼接为字符串（对标 Scala List.mkString）。
;;
;; 语法
;; ----
;; ($ xs :mk-string)
;; ($ xs :mk-string sep)
;; ($ xs :mk-string start sep end)
;;
;; 参数
;; ----
;; sep : string?
;; 元素间分隔符，默认空。
;;
;; start, end : string?
;; 前缀与后缀。
;;
;; 返回值
;; ------
;; string?
;; 拼接结果；空表返回 ""（带 sep 时）或 start+end（带三参时）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :mk-string) => "123")
(check ($ '(1 2 3) :mk-string ",") => "1,2,3")
(check ($ '(1 2 3) :mk-string "[" "," "]") => "[1,2,3]")
(check ($ '() :mk-string ",") => "")
(check ($ '("a" "b") :mk-string "|") => "a|b")
(check ($ '(1 2 3) :filter odd? :map (* _ 10) :mk-string "+") => "10+30")

(check-report)
