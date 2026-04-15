(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-xor!
;; 与 iset-xor 相同，但可以修改第一个集合。
;;
;; 语法
;; ----
;; (iset-xor! iset1 iset2)
;;
;; 参数
;; ----
;; iset1 : iset
;; 要修改的集合。
;;
;; iset2 : iset
;; 用于求对称差集的集合。
;;
;; 返回值
;; -----
;; 返回修改后的原 iset。
;;
(check (iset->list (iset-xor! (iset 0 1 3) (iset 0 2 4))
       ) ;iset->list
  =>
  '(1 2 3 4)
) ;check


(check-report)
