;; list%indices
;; 获取集合从 0 开始的全部合法索引列表（对标 Scala List.indices）。
;;
;; 语法
;; ----
;; ($ xs :indices)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; list?
;; (0 1 ... len-1)；空表返回 '()。支持续链。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :indices) => '(0 1 2))
(check ($ '() :indices) => '())
(check ($ '("a" "b" "c") :indices :map (* _ 10)) => '(0 10 20))

(check-report)
