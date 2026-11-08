;; list%count
;; 统计满足谓词的元素个数（对标 Scala List.count）。
;;
;; 语法
;; ----
;; ($ xs :count p)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; p : procedure?
;; 单参谓词函数；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; integer?
;; 满足 (p x) 为真的元素个数。
;;
;; 描述
;; ----
;; 去糖为 (count p xs)。空集或不满足任何元素时返回 0。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3 4) :count odd?) => 2)
(check ($ '() :count odd?) => 0)
(check ($ '(1 2 3 4 5) :count (<= _ 3)) => 3)

(check-report)
