(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-difference!
;; 与 iset-difference 相同，但可以修改第一个集合。
;;
;; 语法
;; ----
;; (iset-difference! iset1 iset2 ...)
;;
;; 参数
;; ----
;; iset1 : iset
;; 要修改的集合。
;;
;; iset2 ... : iset
;; 要减去的集合。
;;
;; 返回值
;; -----
;; 返回修改后的原 iset。
;;
(check (iset->list (iset-difference! (iset 0 1 3 4)
                     (iset 0 2)
                     (iset 0 4)
                   ) ;iset-difference!
       ) ;iset->list
  =>
  '(1 3)
) ;check


(check-report)
