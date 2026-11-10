;; flexvector-skip-right
;; 从右跳过满足条件的元素。
;;
;; 语法
;; ----
;; (flexvector-skip-right pred? fv)
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
;; 最右侧不满足谓词的索引；全部满足时返回 #f。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(let ((fv (flexvector 10 20 30)))
  ;; 从右跳过大于 10 的元素，剩下最右侧不满足的元素位于索引 0
  (check (flexvector-skip-right (lambda (x) (> x 10)) fv) => 0)
) ;let

(let ((fv (flexvector 10 20 30)))
  ;; 全部元素都满足谓词时返回 #f
  (check-false (flexvector-skip-right (lambda (x) (> x 0)) fv))
) ;let

(let ((fv (flexvector 10 20 30)))
  ;; 从右跳过小于 30 的元素，剩下最右侧不满足的元素位于索引 2
  (check (flexvector-skip-right (lambda (x) (< x 30)) fv) => 2)
) ;let

(check-report)
