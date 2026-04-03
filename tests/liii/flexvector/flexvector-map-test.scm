(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-map
;; 对 flexvector 每个元素应用函数，返回新向量。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-map proc fv)
;; (flexvector-map proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : procedure
;;   接受一个或多个参数的函数。
;;   单向量时: (proc element)
;;   多向量时: (proc element1 element2 ...)
;;
;; fv, fv1, fv2 ... : flexvector
;;   源向量。多向量时长度取最短。
;;
;; 返回值
;; -----
;; 返回新的 flexvector，包含 proc 的返回值。
;; 原向量不被修改。
;;
;; 另见
;; ----
;; flexvector-map! - 原地映射
;; flexvector-map/index - 带索引映射

;; 基本映射
(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector (flexvector-map (lambda (x) (* x 10)) fv))
         => #(100 200 300))
  ;; 原向量不变
  (check (flexvector->vector fv) => #(10 20 30)))

;; 映射到不同类型
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->list (flexvector-map number->string fv))
         => '("1" "2" "3")))

;; 空向量
(check (flexvector->vector (flexvector-map (lambda (x) (* x 2)) (flexvector)))
       => #())

;; 多向量映射（元素对位操作）
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30)))
  (check (flexvector->vector (flexvector-map + fv1 fv2))
         => #(11 22 33)))

;; 多向量长度不同时，结果长度与第一个向量相同
;; 超出其他向量长度的位置保持原值
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20)))
  (check (flexvector->vector (flexvector-map * fv1 fv2))
         => #(10 40 3 4)))

;; 三向量映射
(let ((fv1 (flexvector 1 2))
      (fv2 (flexvector 10 20))
      (fv3 (flexvector 100 200)))
  (check (flexvector->vector (flexvector-map + fv1 fv2 fv3))
         => #(111 222)))

;; 返回新对象
(let ((fv (flexvector 1 2 3)))
  (check (eq? fv (flexvector-map (lambda (x) x) fv)) => #f))

(check-report)
