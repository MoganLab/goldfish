(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector=?
;; 比较两个或多个可变长向量是否相等。
;;
;; 语法
;; ----
;; (flexvector=? eq? fv1 fv2 ...)
;;
;; 参数
;; ----
;; eq? : function
;; 元素比较函数。
;;
;; fv1, fv2, ... : flexvector
;; 要比较的向量。
;;
;; 返回值
;; ----
;; boolean
;; 如果所有向量相等返回 #t，否则返回 #f。
;;
;; 描述
;; ----
;; 使用给定的等价函数比较两个或多个 flexvector 的元素。

(check-true (flexvector=? eq? (flexvector 'a 'b) (flexvector 'a 'b)))
(check-false (flexvector=? eq? (flexvector 'a 'b) (flexvector 'b 'a)))
(check-false (flexvector=? = (flexvector 1 2 3 4 5) (flexvector 1 2 3 4)))
(check-true (flexvector=? eq?))
(check-true (flexvector=? eq? (flexvector 'a)))
(check-true (flexvector=? eq? (flexvector 1 2) (flexvector 1 2) (flexvector 1 2)))
(check-false (flexvector=? eq? (flexvector 1 2) (flexvector 1 2) (flexvector 1 3)))

(check-report)
