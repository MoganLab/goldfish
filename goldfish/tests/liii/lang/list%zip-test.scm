;; list%zip
;; 与另一集合按短侧配对（对标 Scala List.zip）。
;;
;; 语法
;; ----
;; ($ xs :zip other)
;;
;; 参数
;; ----
;; other : list? / vector? / string?
;; 另一集合，可与 xs 异构。
;;
;; 返回值
;; ------
;; list?
;; ((a1 b1) (a2 b2) ...) 二元组链表，长度取两者较短者。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :zip '(4 5 6)) => '((1 4) (2 5) (3 6)))
(check ($ '(1 2) :zip '(3)) => '((1 3)))
(check ($ '(1) :zip '()) => '())
(check ($ '() :zip '(1)) => '())
(check ($ '(1 2) :zip #(3 4)) => '((1 3) (2 4)))

(check-report)
