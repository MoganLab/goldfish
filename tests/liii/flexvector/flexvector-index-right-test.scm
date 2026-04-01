(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-index-right
;; 从右查找第一个满足条件的元素索引。
;;
;; 语法
;; ----
;; (flexvector-index-right pred? fv)
;;
;; 参数
;; ----
;; pred? : function
;; 谓词函数。
;;
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; ----
;; exact-nonnegative-integer 或 #f
;; 从右起第一个满足条件的索引，如果没有则返回 #f。
;;
;; 描述
;; ----
;; 从右到左遍历，返回第一个满足谓词的元素的索引。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector-index-right (lambda (x) (> x 10)) fv) => 2)
  (check (flexvector-index-right (lambda (x) (> x 5)) fv) => 2)
  (check (flexvector-index-right (lambda (x) (> x 50)) fv) => #f)
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector-index-right even? fv) => 3)
  (check (flexvector-index-right (lambda (x) (< x 4)) fv) => 2)
) ;let

(check-report)
