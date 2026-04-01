(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-skip
;; 跳过满足条件的元素，返回第一个不满足条件的索引。
;;
;; 语法
;; ----
;; (flexvector-skip pred? fv)
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
;; 第一个不满足条件的索引，如果都满足则返回 #f。
;;
;; 描述
;; ----
;; 与 flexvector-index 相反，返回第一个不满足谓词的元素的索引。

(let ((fv (flexvector 10 20 30)))
  (check (flexvector-skip (lambda (x) (< x 25)) fv) => 2)
  (check (flexvector-skip (lambda (x) (< x 5)) fv) => 0)
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector-skip odd? fv) => 1)
  (check (flexvector-skip (lambda (x) (< x 10)) fv) => #f)
) ;let

(check-report)
