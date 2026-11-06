;; flexvector-filter!
;; 破坏性过滤操作。
;;
;; 语法
;; ----
;; (flexvector-filter! pred? fv)
;;
;; 参数
;; ----
;; pred? : procedure
;; 谓词过程，接收元素并返回布尔值。
;;
;; fv : flexvector
;; 待过滤的可变长向量。
;;
;; 返回值
;; ----
;; flexvector
;; 过滤后的 flexvector，原对象被就地修改。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)))
  (flexvector-filter! (lambda (x) (< x 25)) fv)
  (check (flexvector->list fv) => '(10 20))
) ;let


(check-report)
