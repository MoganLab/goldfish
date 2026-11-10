;; list%sum
;; 数值求和（对标 Scala List.sum）。
;;
;; 语法
;; ----
;; ($ xs :sum)
;;
;; 参数
;; ----
;; xs : list?
;; 元素为数值的链表。
;;
;; 返回值
;; ------
;; number?
;; 全部元素之和。
;;
;; 描述
;; ----
;; 去糖为原生累加 helper。空集返回 0，与 Scala 一致。
;; 常作为链尾终结算子。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :sum) => 6)
(check ($ '() :sum) => 0)
(check ($ '(1 2 3 4 5) :filter odd? :sum) => 9)
(check ($ '(1 2 3) :map (* _ 2) :sum) => 12)

(check-report)
