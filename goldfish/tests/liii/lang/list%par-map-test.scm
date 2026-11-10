;; list%par-map / list%par-filter / list%par-for-each
;; 多核并行算子（对标 Scala Parallel Collections，基于 (liii par) CSP）。
;;
;; 语法
;; ----
;; ($ xs :par-map f)
;; ($ xs :par-filter p)
;; ($ xs :par-for-each f)
;;
;; 参数
;; ----
;; f : procedure?
;; 单参变换函数；可用 `_` 占位符简写。
;;
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; :par-map/:par-filter —— 与串行 :map/:filter 同构的新集合（顺序保持）；
;; :par-for-each —— 返回原集合（可链）。
;;
;; 描述
;; ----
;; 仅 list/vector 支持；谓词建议写 lambda 形式（内置谓词如 even?
;; 直接传入时，go worker 可能无法提取内建过程源码，会报 type-error）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3 4 5 6) :par-map (* _ 2)) => '(2 4 6 8 10 12))
(check ($ '(1 2 3 4 5 6) :par-filter (lambda (x) (even? x))) => '(2 4 6))
(check ($ '(1 2 3) :par-map (* _ 10) :sum) => 60)
(check ($ '(3 1 2) :par-map (* _ 2) :sort-with <) => '(2 4 6))
(check ($ '() :par-map (lambda (x) x)) => '())

(check-report)
