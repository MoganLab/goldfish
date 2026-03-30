(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-fold
;; 对集合进行折叠操作（按递增顺序）。
;;
;; 语法
;; ----
;; (iset-fold proc nil iset)
;;
;; 参数
;; ----
;; proc : procedure
;; 接受元素和累积值，返回新累积值的函数。
;;
;; nil : any
;; 初始累积值。
;;
;; iset : iset
;; 目标集合。
;;
(check (iset-fold + 0 (iset 2 3 5 7 11)) => 28)
(check (iset-fold cons '() (iset 2 3 5 7 11)) => '(11 7 5 3 2))

(check-report)
