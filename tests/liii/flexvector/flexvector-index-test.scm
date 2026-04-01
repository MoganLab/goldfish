(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-index
;; 查找第一个满足条件的元素索引。
;;
;; 语法
;; ----
;; (flexvector-index pred? fv)
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
;; 第一个满足条件的索引，如果没有则返回 #f。
;;
;; 描述
;; ----
;; 从左到右遍历，返回第一个满足谓词的元素的索引。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector-index (lambda (x) (> x 10)) fv) => 1)
  (check (flexvector-index (lambda (x) (> x 5)) fv) => 0)
  (check (flexvector-index (lambda (x) (> x 50)) fv) => #f)
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector-index even? fv) => 1)
  (check (flexvector-index (lambda (x) (> x 4)) fv) => 4)
) ;let

(check-report)
