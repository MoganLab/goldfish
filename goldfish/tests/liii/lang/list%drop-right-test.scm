;; list%drop-right
;; 去掉尾部 n 个元素，越界宽容（对标 Scala List.dropRight）。
;;
;; 语法
;; ----
;; ($ xs :drop-right n)
;;
;; 参数
;; ----
;; n : integer?
;; 去掉的尾部元素个数，越界/负数宽容。
;;
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; list?
;; 去掉尾部 n 个后的剩余；n 超过长度返回 '()，n 为负返回全部。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :drop-right 2) => '(1))
(check ($ '(1 2) :drop-right -1) => '(1 2))
(check ($ '(1 2 3) :drop-right 10) => '())
(check ($ '() :drop-right 3) => '())

(check-report)
