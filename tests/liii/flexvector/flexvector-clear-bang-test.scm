(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-clear!
;; 清空可变长向量的所有元素。
;;
;; 语法
;; ----
;; (flexvector-clear! fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; ----
;; flexvector
;; 返回清空的 flexvector。
;;
;; 描述
;; ----
;; 移除向量中的所有元素，使其变为空向量。

(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-clear! fv)
  (check (flexvector-length fv) => 0)
  (check (flexvector-empty? fv) => #t)
) ;let

(let ((fv (flexvector)))
  (flexvector-clear! fv)
  (check (flexvector-empty? fv) => #t)
) ;let

(check-report)
