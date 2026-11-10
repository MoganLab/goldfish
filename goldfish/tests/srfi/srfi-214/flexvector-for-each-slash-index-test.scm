;; flexvector-for-each/index
;; 带索引遍历可变长向量。
;;
;; 语法
;; ----
;; (flexvector-for-each/index proc fv)
;; (flexvector-for-each/index proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : procedure
;; 遍历过程，接收索引与元素。
;;
;; fv : flexvector
;; 待遍历的可变长向量。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)) (res '()))
  (flexvector-for-each/index (lambda (i x) (set! res (cons (+ x (* i 2)) res)))
    fv
  ) ;flexvector-for-each/index
  (check res => '(34 22 10))
) ;let


(check-report)
