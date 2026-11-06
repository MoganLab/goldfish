;; flexvector-append-map/index
;; 带索引映射并把映射结果拼接为一个新的可变长向量。
;;
;; 语法
;; ----
;; (flexvector-append-map/index proc fv)
;;
;; 参数
;; ----
;; proc : procedure
;; 带索引映射的过程，接收索引与元素，返回一个 flexvector。
;;
;; fv : flexvector
;; 待映射的可变长向量。
;;
;; 返回值
;; ----
;; flexvector
;; 由映射结果按顺序拼接得到的新可变长向量。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)))
  (check (flexvector->list (flexvector-append-map/index (lambda (i x) (flexvector x (* x 10))) fv))
    =>
    '(10 100 20 200 30 300)
  ) ;check
) ;let

;; 映射过程可以使用索引

(let ((fv (flexvector 10 20 30)))
  (check (flexvector->list (flexvector-append-map/index (lambda (i x) (flexvector (+ x i))) fv))
    =>
    '(10 21 32)
  ) ;check
) ;let

(check-report)
