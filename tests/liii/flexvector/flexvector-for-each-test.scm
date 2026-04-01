(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-for-each
;; 遍历可变长向量的元素。
;;
;; 语法
;; ----
;; (flexvector-for-each proc fv)
;; (flexvector-for-each proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : function
;; 接受一个或多个参数的函数。
;;
;; fv, fv1, fv2, ... : flexvector
;; 要遍历的向量。
;;
;; 返回值
;; ----
;; undefined
;; 返回值未指定。
;;
;; 描述
;; ----
;; 对向量中的每个元素应用给定的函数，从左到右遍历。

(let ((fv (flexvector 10 20 30))
      (res '()))
  (flexvector-for-each (lambda (x) (set! res (cons x res))) fv)
  (check res => '(30 20 10))
) ;let

(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30))
      (res '()))
  (flexvector-for-each (lambda (x y) (set! res (cons (+ x y) res))) fv1 fv2)
  (check res => '(33 22 11))
) ;let

(check-report)
