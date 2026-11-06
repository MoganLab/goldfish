;; flexvector-map/index!
;; 带索引破坏性映射操作。
;;
;; 语法
;; ----
;; (flexvector-map/index! proc fv)
;; (flexvector-map/index! proc fv1 fv2 ...)
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
;; 映射后的 flexvector，原对象被就地修改。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)))
  (flexvector-map/index! (lambda (i x) (+ x (* i 2))) fv)
  (check (flexvector->list fv) => '(10 22 34))
) ;let

;; 原地修改：返回的是同一个对象

(let ((fv (flexvector 1 2 3)))
  (check-true (eq? (flexvector-map/index! (lambda (i x) x) fv) fv))
) ;let

(check-report)
