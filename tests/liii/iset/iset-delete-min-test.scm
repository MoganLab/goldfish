(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-delete-min
;; 返回最小元素和剩余集合。
;;
;; 语法
;; ----
;; (iset-delete-min iset)
;;
;; 返回值
;; -----
;; 返回两个值：最小元素和包含其余元素的新集合。
;; 如果集合为空则报错。
;;
(let-values (((n set) (iset-delete-min (iset 2 3 5 7 11))))
  (check n => 2)
  (check (iset->list set) => '(3 5 7 11))
) ;let-values

(check-report)
