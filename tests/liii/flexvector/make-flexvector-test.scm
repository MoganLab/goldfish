(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; make-flexvector
;; 创建指定大小的可变长向量。
;;
;; 语法
;; ----
;; (make-flexvector size)
;; (make-flexvector size fill)
;;
;; 参数
;; ----
;; size : exact-nonnegative-integer
;; 向量的初始容量。
;;
;; fill : any (可选)
;; 填充值，默认为未指定。
;;
;; 返回值
;; ----
;; flexvector
;; 返回包含 size 个元素的新 flexvector。
;;
;; 描述
;; ----
;; 创建一个指定大小的 flexvector。如果提供了 fill 参数，
;; 所有元素都被初始化为该值。

(check (flexvector-length (make-flexvector 3)) => 3)
(check (flexvector-length (make-flexvector 0)) => 0)
(check (flexvector-length (make-flexvector 3 #f)) => 3)
(check (flexvector->vector (make-flexvector 3 'a)) => #(a a a))
(check (flexvector->vector (make-flexvector 5 0)) => #(0 0 0 0 0))

(check-report)
