(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-intersection!
;; 与 iset-intersection 相同，但可以修改第一个集合。
;;
;; 语法
;; ----
;; (iset-intersection! iset1 iset2 ...)
;;
;; 参数
;; ----
;; iset1 : iset
;; 要修改的集合。
;;
;; iset2 ... : iset
;; 用于求交集的集合。
;;
;; 返回值
;; -----
;; 返回修改后的原 iset。
;;
(check (iset->list (iset-intersection! (iset 0 1 3 4)
                     (iset 0 2 4)
                   ) ;iset-intersection!
       ) ;iset->list
  =>
  '(0 4)
) ;check


(check-report)
