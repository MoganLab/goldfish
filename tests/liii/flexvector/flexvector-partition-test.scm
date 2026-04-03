(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-partition
;; 将向量分为两部分：满足和不满足条件的元素。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-partition pred? fv)
;;
;; 参数
;; ----
;; pred? : procedure
;;   谓词函数。
;;
;; fv : flexvector
;;   源向量。
;;
;; 返回值
;; -----
;; 返回两个值（使用 let-values 接收）：
;;   1. 满足 pred? 的元素组成的新 flexvector
;;   2. 不满足 pred? 的元素组成的新 flexvector
;;
;; 原向量不被修改。
;;
;; 另见
;; ----
;; flexvector-filter - 只保留满足条件的
;; flexvector-remove! - 删除指定位置元素

;; 基本分区
(let ((fv (flexvector 10 20 30)))
  (let-values (((low high) (flexvector-partition (lambda (x) (< x 25)) fv)))
    (check (flexvector->vector low) => #(10 20))
    (check (flexvector->vector high) => #(30))
    ;; 原向量不变
    (check (flexvector->vector fv) => #(10 20 30))))

;; 分偶数和奇数
(let ((fv (flexvector 1 2 3 4 5 6)))
  (let-values (((evens odds) (flexvector-partition even? fv)))
    (check (flexvector->list evens) => '(2 4 6))
    (check (flexvector->list odds) => '(1 3 5))))

;; 全部满足
(let ((fv (flexvector 2 4 6)))
  (let-values (((yes no) (flexvector-partition even? fv)))
    (check (flexvector->vector yes) => #(2 4 6))
    (check (flexvector->vector no) => #())))

;; 全部不满足
(let ((fv (flexvector 1 3 5)))
  (let-values (((yes no) (flexvector-partition even? fv)))
    (check (flexvector->vector yes) => #())
    (check (flexvector->vector no) => #(1 3 5))))

;; 空向量
(let-values (((yes no) (flexvector-partition (lambda (x) #t) (flexvector))))
  (check (flexvector->vector yes) => #())
  (check (flexvector->vector no) => #()))

;; 单元素满足
(let ((fv (flexvector 42)))
  (let-values (((yes no) (flexvector-partition (lambda (x) (= x 42)) fv)))
    (check (flexvector->vector yes) => #(42))
    (check (flexvector->vector no) => #())))

;; 单元素不满足
(let ((fv (flexvector 42)))
  (let-values (((yes no) (flexvector-partition (lambda (x) (= x 0)) fv)))
    (check (flexvector->vector yes) => #())
    (check (flexvector->vector no) => #(42))))

;; 保持相对顺序
(let ((fv (flexvector 1 2 3 4 5)))
  (let-values (((yes no) (flexvector-partition (lambda (x) (> x 3)) fv)))
    ;; 大于3的元素按原顺序: 4, 5
    (check (flexvector->list yes) => '(4 5))
    ;; 不大于3的元素按原顺序: 1, 2, 3
    (check (flexvector->list no) => '(1 2 3))))

(check-report)
