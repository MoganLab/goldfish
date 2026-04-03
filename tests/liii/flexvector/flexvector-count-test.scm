(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-count
;; 统计满足条件的元素数量。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-count pred? fv)
;; (flexvector-count pred? fv ...)
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
;; 返回满足 pred? 的元素个数（精确非负整数）。
;;
;; 另见
;; ----
;; flexvector-any - 检查是否存在
;; flexvector-every - 检查是否全部

;; 基本测试
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-count (lambda (x) (< x 25)) fv) => 2))

;; 统计偶数
(let ((fv (flexvector 1 2 3 4 5 6)))
  (check (flexvector-count even? fv) => 3))

;; 全部满足
(let ((fv (flexvector 2 4 6)))
  (check (flexvector-count even? fv) => 3))

;; 全部不满足
(let ((fv (flexvector 1 3 5)))
  (check (flexvector-count even? fv) => 0))

;; 空向量
(check (flexvector-count (lambda (x) #t) (flexvector)) => 0)

;; 单元素满足
(let ((fv (flexvector 42)))
  (check (flexvector-count (lambda (x) (= x 42)) fv) => 1))

;; 单元素不满足
(let ((fv (flexvector 42)))
  (check (flexvector-count (lambda (x) (= x 0)) fv) => 0))

;; 多向量版本
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20 30 1)))
  (check (flexvector-count (lambda (x y) (< x y)) fv1 fv2) => 3))

;; 多向量长度不同
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20)))
  (check (flexvector-count (lambda (x y) (< x y)) fv1 fv2) => 2))

(check-report)
