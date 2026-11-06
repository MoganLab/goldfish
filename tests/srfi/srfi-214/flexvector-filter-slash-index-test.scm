;; flexvector-filter/index
;; 带索引过滤操作。
;;
;; 语法
;; ----
;; (flexvector-filter/index pred? fv)
;;
;; 参数
;; ----
;; pred? : procedure
;; 谓词过程，接收索引与元素并返回布尔值。
;;
;; fv : flexvector
;; 待过滤的可变长向量。
;;
;; 返回值
;; ----
;; flexvector
;; 过滤后得到的新 flexvector。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector (flexvector-filter/index (lambda (i x) (not (= i 1))) fv))
    =>
    #(10 30)
  ) ;check
) ;let


(check-report)
