(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-map
;; 对可变长向量进行映射操作，返回新向量。
;;
;; 语法
;; ----
;; (flexvector-map proc fv)
;; (flexvector-map proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : function
;; 映射函数。
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
;; 对向量元素应用函数，返回包含结果的新 flexvector。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector (flexvector-map (lambda (x) (* x 10)) fv))
         => #(100 200 300)
  ) ;check
) ;let

(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30)))
  (check (flexvector->vector (flexvector-map + fv1 fv2))
         => #(11 22 33)
  ) ;check
) ;let

(check-report)
