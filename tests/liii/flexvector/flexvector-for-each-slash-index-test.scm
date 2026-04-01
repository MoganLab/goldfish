(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-for-each/index
;; 带索引遍历可变长向量的元素。
;;
;; 语法
;; ----
;; (flexvector-for-each/index proc fv)
;; (flexvector-for-each/index proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : function
;; 接受索引和元素作为参数的函数。
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
;; 对向量中的每个元素应用给定的函数，函数接收索引和元素值。

(let ((fv (flexvector 10 20 30))
      (res '()))
  (flexvector-for-each/index
    (lambda (i x) (set! res (cons (+ x (* i 2)) res)))
    fv
  ) ;flexvector-for-each/index
  (check res => '(34 22 10))
) ;let

(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30))
      (res '()))
  (flexvector-for-each/index
    (lambda (i x y) (set! res (cons (+ i x y) res)))
    fv1 fv2
  ) ;flexvector-for-each/index
  (check res => '(35 23 11))
) ;let

(check-report)
