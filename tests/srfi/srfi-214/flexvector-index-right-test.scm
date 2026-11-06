;; flexvector-index-right
;; 从右查找元素索引。
;;
;; 语法
;; ----
;; (flexvector-index-right pred? fv)
;;
;; 参数
;; ----
;; pred? : procedure
;; 谓词过程，接收元素并返回布尔值。
;;
;; fv : flexvector
;; 待查找的可变长向量。
;;
;; 返回值
;; ----
;; exact-integer
;; 最右侧满足谓词的索引；没有匹配元素时返回 #f。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)))
  (check (flexvector-index-right (lambda (x) (> x 10)) fv) => 2)
) ;let


(check-report)
