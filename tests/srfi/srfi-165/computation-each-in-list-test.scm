;; computation-each-in-list
;; 依次执行列表中的计算，忽略各自的结果。
;;
;; 语法
;; ----
;; (computation-each-in-list computations)
;;
;; 参数
;; ----
;; computations : list
;; 待执行的计算对象列表。
;; 返回值
;; ----
;; computation
;; 返回依次执行这些计算的新计算对象。
;; 说明
;; ----
;; 与 computation-each 的区别是参数为列表形式；
;; 列表不能为空。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(check (computation-run (computation-each-in-list (list (computation-pure 'a)
                                                        (computation-pure 'b)
                                                        (computation-pure 'c)
                                                      ) ;list
                        ) ;computation-each-in-list
       ) ;computation-run
  =>
  'c
) ;check

(check-report)
