(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-length
;; 返回可变长向量的长度。
;;
;; 语法
;; ----
;; (flexvector-length fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; ----
;; exact-nonnegative-integer
;; 向量的元素个数。
;;
;; 描述
;; ----
;; 返回 flexvector 中元素的数量。

(check (flexvector-length (flexvector)) => 0)
(check (flexvector-length (flexvector 1 2 3)) => 3)
(check (flexvector-length (make-flexvector 10)) => 10)
(check (flexvector-length (flexvector 'a 'b 'c 'd 'e)) => 5)

(let ((fv (flexvector 1 2)))
  (flexvector-add-back! fv 3)
  (check (flexvector-length fv) => 3)
) ;let

(check-report)
