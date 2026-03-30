(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; make-flexvector
;; 创建一个新的可变长向量。
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
;; fill : any
;; 填充值。
;;
;; 返回值
;; -----
;; 返回包含指定元素的新 flexvector。
;;
(check (flexvector-length (make-flexvector 3 #f)) => 3)
(check (flexvector->vector (make-flexvector 3 'a)) => #(a a a))

(check-report)
