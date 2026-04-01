(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-map/index
;; 带索引映射可变长向量。
;;
;; 语法
;; ----
;; (flexvector-map/index proc fv)
;; (flexvector-map/index proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : function
;; 接收索引和元素作为参数的映射函数。
;;
;; fv, fv1, fv2, ... : flexvector
;; 源向量。
;;
;; 返回值
;; ----
;; flexvector
;; 包含映射结果的新向量。
;;
;; 描述
;; ----
;; 对向量元素应用函数，函数接收索引和元素值。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-map/index (lambda (i x) (+ x (* i 2))) fv))
         => #(10 22 34)
  ) ;check
) ;let

(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-map/index (lambda (i x y) (+ i x y)) fv1 fv2))
         => #(11 23 35)
  ) ;check
) ;let

(check-report)
