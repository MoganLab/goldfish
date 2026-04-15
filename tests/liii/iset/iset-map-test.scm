(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-map
;; 对集合中的每个元素应用函数，并返回一个新集合。
;;
;; 语法
;; ----
;; (iset-map proc iset)
;;
;; 参数
;; ----
;; proc : procedure
;; 接受一个整数并返回整数的函数。
;;
;; iset : iset
;; 源集合。
;;
;; 返回值
;; -----
;; 返回新的 iset，包含 proc 的结果。
;; 注意：如果 proc 返回非整数会报错；如果产生重复元素会被去重。
;;
(check-true (iset=? (iset-map (lambda (x) (* 10 x))
                      (iset 1 11 21)
                    ) ;iset-map
              (iset 10 110 210)
            ) ;iset=?
) ;check-true
(check (iset->list (iset-map (lambda (x) (quotient x 2))
                     (iset 1 2 3 4 5)
                   ) ;iset-map
       ) ;iset->list
  =>
  '(0 1 2)
) ;check


(check-report)
