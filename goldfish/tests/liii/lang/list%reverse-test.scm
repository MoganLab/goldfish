;; list%reverse
;; 反转集合（对标 Scala List.reverse）。
;;
;; 语法
;; ----
;; ($ xs :reverse)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; list?
;; 元素顺序完全颠倒的新链表。
;;
;; 描述
;; ----
;; 去糖为 (reverse xs)。不修改原链表；空表返回空表。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :reverse) => '(3 2 1))
(check ($ '() :reverse) => '())
(check ($ '(1 2 3) :reverse :reverse) => '(1 2 3))

(check-report)
