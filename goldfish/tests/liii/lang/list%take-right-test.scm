;; list%take-right
;; 取尾部 n 个元素，越界宽容（对标 Scala List.takeRight）。
;;
;; 语法
;; ----
;; ($ xs :take-right n)
;;
;; 参数
;; ----
;; n : integer?
;; 保留的尾部元素个数，越界/负数宽容。
;;
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; list?
;; 尾部 n 个元素；n 超过长度返回全部，n 为负或 0 返回 '()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :take-right 2) => '(2 3))
(check ($ '(1 2) :take-right 5) => '(1 2))
(check ($ '(1 2 3) :take-right 0) => '())
(check ($ '() :take-right 3) => '())

(check-report)
