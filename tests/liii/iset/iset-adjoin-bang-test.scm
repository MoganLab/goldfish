(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-adjoin!
;; 与 iset-adjoin 相同，但可以修改原集合。
;;
;; 语法
;; ----
;; (iset-adjoin! iset element ...)
;;
;; 参数
;; ----
;; iset : iset
;; 要修改的集合。
;;
;; element ... : exact-integer
;; 要添加的元素。
;;
;; 返回值
;; -----
;; 返回修改后的原 iset。
;;
(check (iset->list (iset-adjoin! (iset 1 3 5) 0)) => '(0 1 3 5))

(check-report)
