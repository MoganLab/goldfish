(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-empty?
;; 检查可变长向量是否为空。
;;
;; 语法
;; ----
;; (flexvector-empty? fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; ----
;; boolean
;; 如果向量为空返回 #t，否则返回 #f。
;;
;; 描述
;; ----
;; 用于快速检查向量是否不包含任何元素。

(check-true (flexvector-empty? (flexvector)))
(check-false (flexvector-empty? (flexvector 1 2 3)))
(check-true (flexvector-empty? (make-flexvector 0)))
(check-false (flexvector-empty? (make-flexvector 1)))

(let ((fv (flexvector 'a)))
  (flexvector-remove-back! fv)
  (check-true (flexvector-empty? fv))
) ;let

(check-report)
