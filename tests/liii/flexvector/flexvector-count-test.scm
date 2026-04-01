(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-count
;; 统计满足条件的元素数量。
;;
;; 语法
;; ----
;; (flexvector-count pred? fv)
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
;; exact-nonnegative-integer
;; 满足条件的元素数量。
;;
;; 描述
;; ----
;; 返回向量中满足谓词的元素个数。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector-count (lambda (x) (< x 25)) fv) => 2)
  (check (flexvector-count (lambda (x) (> x 15)) fv) => 2)
  (check (flexvector-count (lambda (x) (> x 50)) fv) => 0)
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector-count odd? fv) => 3)
  (check (flexvector-count even? fv) => 2)
) ;let

(check-report)
