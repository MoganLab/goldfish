(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-union!
;; 与 iset-union 相同，但可以修改第一个集合。
;;
;; 语法
;; ----
;; (iset-union! iset1 iset2 ...)
;;
;; 参数
;; ----
;; iset1 : iset
;; 要修改的集合。
;;
;; iset2 ... : iset
;; 用于求并集的集合。
;;
;; 返回值
;; -----
;; 返回修改后的原 iset。
;;
(check (iset->list (iset-union! (iset 0 1 3) (iset 0 2 4)))
       => '(0 1 2 3 4)
) ;check

(check-report)
