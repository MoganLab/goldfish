;; flexvector-append-map
;; 映射并把映射结果拼接为一个新的可变长向量。
;;
;; 语法
;; ----
;; (flexvector-append-map proc fv)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射过程，接收元素，返回一个 flexvector。
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


(check (flexvector->vector (flexvector-append-map (lambda (x) (flexvector x (* x 10)))
                             (flexvector 10 20 30)
                           ) ;flexvector-append-map
       ) ;flexvector->vector
  =>
  #(10 100 20 200 30 300)
) ;check


(check-report)
