;; list%starts-with? / list%ends-with?
;; 判断是否以指定前缀/后缀开始或结尾（对标 Scala List.startsWith / endsWith）。
;;
;; 语法
;; ----
;; ($ xs :starts-with? prefix)
;; ($ xs :ends-with? suffix)
;;
;; 参数
;; ----
;; prefix : list?
;; 前缀链表。
;;
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; boolean
;; 空前缀/后缀恒为 #t；超长返回 #f。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :starts-with? '(1 2)) => #t)
(check ($ '(1 2 3) :starts-with? '()) => #t)
(check ($ '(1 2 3) :starts-with? '(2)) => #f)
(check ($ '(1 2 3) :starts-with? '(1 2 3 4)) => #f)

(check ($ '(1 2 3) :ends-with? '(2 3)) => #t)
(check ($ '(1 2 3) :ends-with? '()) => #t)
(check ($ '(1 2 3) :ends-with? '(1)) => #f)

(check-report)
