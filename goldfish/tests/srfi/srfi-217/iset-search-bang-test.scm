;;
;; iset-search!
;; 与 iset-search 相同，但可以修改原集合。
;;

;; 语法
;; ----
;; (iset-search! set elt failure success)

;; 参数
;; ----
;; set : iset
;; 目标集合。
;;
;; elt : exact-integer
;; 待查找的元素。
;;
;; failure : procedure
;; 查找失败时调用的过程，接收插入与忽略两个分支过程。
;;
;; success : procedure
;; 查找成功时调用的过程，接收键、更新与删除分支过程。

;; 返回值
;; ----
;; values
;; 返回两个值：更新后的 iset 与查找到的对象。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define mixed-set (list->iset (iota 20 -10 3)))


(call-with-values (lambda ()
                    (iset-search! (iset-copy mixed-set)
                      1
                      (lambda (insert _) (insert #t))
                      (lambda (x update _) (update 1 #t))
                    ) ;iset-search!
                  ) ;lambda
  (lambda (set _) (check-true (iset=? (iset-adjoin mixed-set 1) set)))
) ;call-with-values


(check-report)
