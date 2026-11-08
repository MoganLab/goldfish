;; variable-comparator
;; 环境变量的比较器。
;;
;; 语法
;; ----
;; variable-comparator
;;
;; 参数
;; ----
;; 无
;; 返回值
;; ----
;; comparator
;; 用于比较环境变量的比较器。
;; 说明
;; ----
;; 环境变量带有唯一编号，该比较器按编号排序，
;; 因此可以用于哈希表等需要一致比较语义的结构。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128) (srfi srfi-165))


(check-set-mode! 'report-failed)


;; 是比较器
(check-true (comparator? variable-comparator))

;; 相等性：同一变量相等，不同变量不等
(define v1 (make-computation-environment-variable 'a 1 #f))
(define v2 (make-computation-environment-variable 'b 2 #f))
(define v3 (make-computation-environment-variable 'c 3 #f))
(check-true (=? variable-comparator v1 v1))
(check-false (=? variable-comparator v1 v2))

;; 排序：不同变量之间恰好有一个方向成立（严格全序）
(check-true (or (<? variable-comparator v1 v2) (<? variable-comparator v2 v1)))
(check-true (or (<? variable-comparator v2 v3) (<? variable-comparator v3 v2)))
(check-false (<? variable-comparator v1 v1))

;; 排序与相等性一致：相等则不严格小于
(check-false (<? variable-comparator v1 v1))


(check-report)
