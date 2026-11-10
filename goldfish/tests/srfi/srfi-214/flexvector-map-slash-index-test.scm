;; flexvector-map/index
;; 带索引映射操作。
;;
;; 语法
;; ----
;; (flexvector-map/index proc fv)
;; (flexvector-map/index proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : procedure
;; 带索引映射的过程，接收索引与元素。
;;
;; fv : flexvector
;; 待映射的可变长向量。
;;
;; 返回值
;; ----
;; flexvector
;; 映射后得到的新 flexvector。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector (flexvector-map/index (lambda (i x) (+ x (* i 2))) fv))
    =>
    #(10 22 34)
  ) ;check
) ;let


(check-report)
