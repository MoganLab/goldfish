(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-append-map
;; 对可变长向量映射并追加结果。
;;
;; 语法
;; ----
;; (flexvector-append-map proc fv)
;; (flexvector-append-map proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : function
;; 返回 flexvector 的映射函数。
;;
;; fv, fv1, fv2, ... : flexvector
;; 源向量。
;;
;; 返回值
;; ----
;; flexvector
;; 连接所有映射结果的新向量。
;;
;; 描述
;; ----
;; 对元素应用函数，函数返回 flexvector，然后将所有结果连接。

(check (flexvector->vector
         (flexvector-append-map (lambda (x) (flexvector x (* x 10)))
                                (flexvector 10 20 30))
         ) ;flexvector-append-map
       => #(10 100 20 200 30 300)
) ;check

(let ((fv (flexvector 1 2 3)))
  (check (flexvector->vector
           (flexvector-append-map (lambda (x) (flexvector x x)) fv))
         => #(1 1 2 2 3 3)
  ) ;check
) ;let

(check-report)
