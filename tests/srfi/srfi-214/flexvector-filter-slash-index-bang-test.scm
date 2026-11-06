;; flexvector-filter/index!
;; 带索引破坏性过滤操作，保留满足谓词的元素。
;;
;; 语法
;; ----
;; (flexvector-filter/index! pred? fv)
;;
;; 参数
;; ----
;; pred? : procedure
;; 谓词过程，接收索引与元素并返回布尔值。
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
  (flexvector-filter/index! (lambda (i x) (not (= i 1))) fv)
  (check (flexvector->list fv) => '(10 30))
) ;let

;; 谓词只依赖索引时同样生效

(let ((fv (flexvector 10 20 30 40)))
  (flexvector-filter/index! (lambda (i x) (even? i)) fv)
  (check (flexvector->list fv) => '(10 30))
) ;let

;; 原地修改：返回的是同一个对象

(let ((fv (flexvector 1 2 3)))
  (check-true (eq? (flexvector-filter/index! (lambda (i x) #t) fv) fv))
) ;let

(check-report)
