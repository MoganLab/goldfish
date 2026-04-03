(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; make-flexvector
;; 创建指定长度的可变长向量，所有元素初始化为 fill 值。
;; 时间复杂度 O(n)，n 为 size。
;;
;; 语法
;; ----
;; (make-flexvector size)
;; (make-flexvector size fill)
;;
;; 参数
;; ----
;; size : exact-nonnegative-integer
;;   向量的初始长度（不是容量）。
;;
;; fill : any (可选，默认未指定)
;;   填充值。如果不提供，元素值未初始化。
;;
;; 返回值
;; -----
;; 返回新的 flexvector，长度为 size。
;;
;; 示例
;; ----
;; ;; 创建空向量
;; (make-flexvector 0)              => #<flexvector>
;; (flexvector-length (make-flexvector 0)) => 0
;;
;; ;; 使用指定值填充
;; (flexvector->vector (make-flexvector 3 'a)) => #(a a a)
;; (flexvector->vector (make-flexvector 3 0))  => #(0 0 0)

;; 基本测试
(check (flexvector-length (make-flexvector 0)) => 0)
(check (flexvector-length (make-flexvector 3)) => 3)
(check (flexvector->vector (make-flexvector 3 #f)) => #(#f #f #f))
(check (flexvector->vector (make-flexvector 3 'a)) => #(a a a))

;; 边界测试：大量元素
(let ((fv (make-flexvector 100 'x)))
  (check (flexvector-length fv) => 100)
  (check (flexvector-ref fv 0) => 'x)
  (check (flexvector-ref fv 99) => 'x))

;; 修改后行为
(let ((fv (make-flexvector 3 'a)))
  (flexvector-set! fv 1 'b)
  (check (flexvector->vector fv) => #(a b a)))

(check-report)
