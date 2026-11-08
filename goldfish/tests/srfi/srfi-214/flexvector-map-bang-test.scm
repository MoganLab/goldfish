;; flexvector-map!
;; 破坏性映射操作。
;;
;; 语法
;; ----
;; (flexvector-map! proc fv)
;; (flexvector-map! proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射过程，接收元素。
;;
;; fv : flexvector
;; 待映射的可变长向量。
;;
;; 返回值
;; ----
;; flexvector
;; 映射后的 flexvector，原对象被就地修改。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)))
  (flexvector-map! (lambda (x) (* x 10)) fv)
  (check (flexvector->list fv) => '(100 200 300))
) ;let


(check-report)
